#include <iostream>
#include <cmath>
#include <R.h>
#include <R_ext/Utils.h>
#include "matrix.h"
#include "la.h"
#include "ide.h"
#include "distributions.h"
#include "rng.h"
#include "stat.h"

// shorthand
typedef unsigned int uint;
typedef scythe::Matrix<double,scythe::Row> matrix;
typedef scythe::Matrix<uint,scythe::Row> imatrix;
typedef scythe::Matrix<bool,scythe::Row> bmatrix;
typedef scythe::Matrix<double,scythe::Row,scythe::View> view;

#define NEWMAX 2
#define NEWMIN -2
#define SIGMASTART 0.2

namespace MROPROBIT {
  namespace {

  
    struct rescale {
      double oldmin, oldmax;

      rescale (double min, double max) : oldmin (min), oldmax(max) {}

      double operator() (double x)
      {
        if (x == 0)
          return NAN;
        return (((NEWMAX - NEWMIN) / (oldmax - oldmin)) * (x - oldmin)) 
          + NEWMIN;
      }
    };
  }

  /* Generates starting values for the matrix of perceived latent
   * traits; that is each of J judges' perceived level of whatever in
   * each of n observational units.  We simply rescale each rater to
   * on -2 to 2 to approximate standard normal.  This gets everyone on
   * the right scale at least, although it is certainly rough.
   *
   * Pass y as the first argument.
   */
  matrix init_perceived (matrix ts, const matrix& C)
  {
    for (uint j = 0; j < ts.cols(); ++j) {
      view column = ts(scythe::_, j);
      std::transform(column.begin_f(), column.end_f(), column.begin_f(),
          rescale(1, C(j)));
    }

    return ts;
  }

  /* Generates starting values for the cutoff matrix.  Same scale on
   * -2 to 2 we use above.
   */

  matrix init_gamma(const imatrix& C)
  {
    uint J = C.size();
    matrix gamma(J, max(C) + 1, false);

    for (uint j = 0; j < J; ++j) {
      gamma(j, 0) = -10; // negative infinity
      gamma(j, C(j)) = 10;  // infinity

      rescale rs(1, C(j));
      for (uint c = 1; c < C(j); ++c)
        gamma(j, c) = rs(c);
    }

    return gamma;
  }

  /* Initializes the vector of judge error variance parameters.
   * Currently use the naive strategy of initializing them all to
   * 0.2.
   */

  matrix init_sigmasq(uint J)
  {
    return matrix(J, 1, true, SIGMASTART);
  }

#define TD_SPREAD 2
#define TD_MIN 1

  /* Disperses by adding or subtracting (at random) a random value
   * between TD_MIN and TD_MIN+TD_SPREAD to each value in t.
   */
  template <typename RNGTYPE>
  void disperse_t (matrix& t, const bmatrix& did_grade,
                   scythe::rng<RNGTYPE>& myrng, pthread_mutex_t* mutex)
  {
    uint n = t.rows();
    uint J = t.cols();
    
    for (uint i = 0; i < n; ++i) {
      for (uint j = 0; j < J; ++j) {
        if (did_grade(i, j)) {
          //if (mutex != NULL) pthread_mutex_lock (mutex);
          double val = myrng.runif()*TD_SPREAD + TD_MIN;
          if (myrng.runif() < .5)
            val *= -1;
          //if (mutex != NULL) pthread_mutex_unlock (mutex);
          t(i,j) += val;
        }
      }

    }
  }

#define GD_SPREAD .5
#define GD_MIN .5
#define GD_ITS 10

  /* Disperses by first shifting all of the gammas up or down (at
   * random) by a random value between GD_MIN and GD_MIN+GD_SPREAD and
   * then setting each cutoff to random value between the cutoff below
   * it and the cutoff above it and repeating this reshuffling process
   * GD_ITS times.  
   */
  template <typename RNGTYPE>
  void disperse_gamma (matrix& gamma, const imatrix& C,
                       scythe::rng<RNGTYPE>& myrng,
                       pthread_mutex_t* mutex)
  {
    uint J = C.size();
    for (uint j = 0; j < J; ++j) {
      //if (mutex != NULL) pthread_mutex_lock (mutex);
      double shift = myrng.runif()*GD_SPREAD+GD_MIN;
      if (myrng.runif() < .5)
        gamma(j, scythe::_) += shift;
      else
        gamma(j, scythe::_) -= shift;
      //if (mutex != NULL) pthread_mutex_unlock (mutex);
      gamma(j, 0) = -10; // negative infinity
      gamma(j, C(j)) = 10;  // infinity

      for (uint i = 0; i < GD_ITS; ++i) {
        for (uint c = 2; c < C(j) - 1; ++c) {
          double below = gamma(j, c-1);
          double above = gamma(j, c+1);
          //if (mutex != NULL) pthread_mutex_lock (mutex);
          gamma(j, c) = myrng.runif()*(above-below)+below;
          //if (mutex != NULL) pthread_mutex_unlock (mutex);
        }
      }
    }
  }


#define SD_SPREAD 2
#define SD_MIN 2

  /* Disperses by randomly multiplying or dividing each value by a
   * random number between SD_MIN and SD_MIN+SD_SPREAD.
   */
  template <typename RNGTYPE>
  void disperse_sigmasq (matrix& sigmasq, scythe::rng<RNGTYPE>& myrng,
                         pthread_mutex_t* mutex)
  {
    for (uint i = 0; i < sigmasq.size(); ++i) {
     // if (mutex != NULL) pthread_mutex_lock (mutex);
      double val = myrng.runif() * SD_SPREAD + SD_MIN;
      if (myrng.runif() < .5)
        sigmasq(i) *= val;
      else
        sigmasq(i) /= val;
      //if (mutex != NULL) pthread_mutex_unlock (mutex);
    }
  }

  /* Draw a sample from the conditional distribution of Z, given
   * {t_ij} and {sigma^2_j}.  Z is conditionally distributed normally
   * with mean s/r and variance 1/r, calculated according to eq. 5.6
   * in Johnson & Albert 1999 (see pg 166).
   */
  template <typename RNGTYPE>
  matrix sample_trait(const matrix& t, const matrix& sigma_sq,
                      const bmatrix& did_grade, const matrix& mu0,
                      const matrix& sigmasq0,
                      scythe::rng<RNGTYPE>& myrng,
                      pthread_mutex_t* mutex)
  {
    uint n = t.rows();
    uint J = t.cols();
    
    matrix Z(n, 1, false); // Create an empty Z col vector

    for (uint i = 0; i < n; ++i) {
      double r = 1 / sigmasq0(i);
      double s = mu0(i) / sigmasq0(i);
      for (uint j = 0; j < J; ++j) {
        if (did_grade(i, j)) {
          r += (1 / sigma_sq[j]);
          s += (t(i,j) / sigma_sq[j]);
        }
      }

      //if (mutex != NULL) pthread_mutex_lock (mutex);
      Z[i] = myrng.rnorm(s/r, sqrt(1/r));
      //if (mutex != NULL) pthread_mutex_unlock (mutex);
    }

    return Z;
  }

  /* Take the current gamma vector and propose the next vector-value for
   * the Metropolis-Hastings algorithm to jump to.
   */
  template <typename RNGTYPE>
  matrix propose_gamma (const matrix& gamma, double sigma_mh, 
                        scythe::rng<RNGTYPE>& myrng,
                        pthread_mutex_t* mutex)
  {
    matrix gamma_p = gamma;
    uint C = gamma.size() - 1;

    for (uint i = 1; i < C; ++i) {
      //if (mutex != NULL) pthread_mutex_lock (mutex);
      try {
      gamma_p[i] = myrng.rtnorm_combo(gamma[i], sigma_mh*sigma_mh, 
                                      gamma_p[i-1], gamma[i+1]);
      } catch (scythe::scythe_exception& e) {
        Rprintf("throw in propose_gamma\n");
        throw;
      }
      //if (mutex != NULL) pthread_mutex_unlock (mutex);
    }

    return (gamma_p);
  }

  /* Calculate the acceptance ratio for the MH step.  Works in terms of
   * the log acceptance ratio because adds are cheap and reduces the
   * probability of overflows.  See Johnson & Albert 1999, pp. 136.
   */
  double accept_ratio(const matrix& gamma_p, const matrix& gamma,
                      const imatrix& y, const matrix& Z,
                      const bmatrix &did_grade, double sigma_j,
                      double sigma_mh)
  {
    uint n = Z.rows();
    uint C = gamma.size() - 1;

    double logratio = 0;


    /* Calculate the log of the first product in eq 4.12 */
    for (uint i = 0; i < n; ++i) {
      if (did_grade[i]) {
        double a =  std::log(scythe::pnorm(gamma_p[y[i]], Z[i], sigma_j) 
                        - scythe::pnorm(gamma_p[y[i]-1], Z[i], sigma_j));
        double b = std::log(scythe::pnorm(gamma[y[i]], Z[i], sigma_j)
                        - scythe::pnorm(gamma[y[i]-1], Z[i], sigma_j));

        logratio += a - b;
      }

    }

    /* Calculate the log of the 2nd product in eq 4.12 */
    for (uint i = 1; i < C; ++i) {
      double a = std::log(scythe::pnorm(gamma[i+1], gamma[i], sigma_mh)
                    - scythe::pnorm(gamma_p[i-1], gamma[i], sigma_mh));
      double b = std::log(scythe::pnorm(gamma_p[i+1],gamma_p[i],
                  sigma_mh)
                  - scythe::pnorm(gamma[i-1], gamma_p[i], sigma_mh));
      logratio += a - b;
    }

    if (std::isnan(logratio)) {
      Rprintf("WARNING: NaN in accept_ratio!\n");
      logratio = 0;
    }

    return std::exp(logratio);
  }

  /* Generate a sample from the posterior distribution of a
   * multi-rater ordered probit model (Johnson & Albert 1999, section
   * 5.2) using a hybrid Metropolis-Hasting / Gibbs sampler, based on
   * Cowles' (1996) data augmentation algorithm for ordered probit.
   *
   * Parameters:
   *
   * sigmasq_store, gamma_store, z_store, t_store - pointers to
   * pre-allocated arrays to hold the posteriors.
   *
   * Y = An nXJ data matrix containing J judges' ordinal (starting at
   * 1) ratings of n somethings.  Missing values should be indicated
   * with number <= 0.
   *
   * C = An 1xJ matrix indicating the number of possible values on the
   * outcome variable (Y_ij) for each judge.  Columns in this matrix
   * correspond to those in Y.
   *
   * myrng = A random number generator
   *
   * alpha = The shape parameter for the prior inverse gamma
   * distribution over the variances.
   *
   * lambda = The scale parameter for the prior inverse gamma
   * distribution over the variances.
   *
   * mu0 = A vector of means for the normal priors over each Z_i.  
   * If this value is a scalar matrix, we assume the same prior mu for 
   * each Z_i.
   *
   * sigmasq0 = A vector of variances for the normal priors over each
   * Z_i.  If this value is a scalar matrix, we assume the same prior
   * sigma squared for each Z_i. This matrix should contain values
   * only >= 0.
   *
   * save_traits = A boolean indicating whether or not to save the
   * estimated latent traits for the cases.  Switching this to false
   * can greatly reduce the size of the returned posterior matrix if
   * your number of cases is large.  Of course, you won't get
   * estimates of the latent traits if you do this...
   *
   * save_perceived_traits = A boolean indicating whether or not to
   * save the estimated judeges perception of cases latent traits.
   * This is a lot of data, so careful with your memory if you decide
   * to turn it on.
   *
   * sigma_mh = The variance for the Metropolis-Hastings step.  Should
   * be a J-length vector, on real>0 for each rater.
   *
   * mcmc_its = The number of mcmc_iterations to sample.
   *
   * burnin = The number of initial iterations to throw out.  
   *
   * thin = The ratio to thin the stream by.  mcmc_its must be
   * evenly divisible by this value.  When the number of cases is
   * large, you will want to thin the chain heavily to save memory.
   * 
   * verbose = Print the state of the chain every verbose iteration.
   *
   * t_start, gamma_start, sigmasq_start = start values for the
   * various parameter matrices.  If null matrices, we intitalize
   * using rough approximations.  Default to null matrices.
   *
   * disperse = if set to true adds shocks to starting values to
   * disperse them.  Useful when running multiple chains to assess
   * convergence.  Defaults to false.
   *
   * Return value:
   *
   * A mcmc_its / thin by Y.cols() + sum(C) + n + n*J matrix that
   * contains mcmc_its / thin draws from the joint posterior
   * distribution.  The first Y.cols() columns contain the judge error
   * variances, the next sum(C) columns contain the judge-specific
   * cutoffs (gammas), while the n colums contain the latent traits
   * for each case (Zs).  The n Zs are optional (see save_traits
   * above).  Finally, the last n*J columns (optionally, see
   * save_perceived_traits above) contain the judges perceived latent
   * traits for each case; that is, the first column of this group
   * holds the posterior sample of judge 1's perceptions of case 1,
   * the second column holds judge 1's perceptions of case 2, and so
   * on.  Missing values in this portion of the return matrix are set
   * to NaN.
   */

  template <typename RNGTYPE>
  int mroprobit (int chain, pthread_mutex_t* mutex,
                 double* sigmasq_store, double* gamma_store,
                 double* z_store, double* t_store,
                 imatrix Y, imatrix C, scythe::rng<RNGTYPE>& myrng,
                 double alpha, double lambda, matrix mu0, matrix
                 sigmasq0, bool save_traits, bool save_perceived_traits,
                 matrix sigma_mh, uint mcmc_its, uint burnin, int thin,
                 int verbose, matrix& t, matrix& gamma, matrix& sigmasq,
                 bool disperse = false)
  {
    //if (mutex != NULL) pthread_mutex_lock (mutex);

    Rprintf("Starting chain %d\n", chain);

    /* Set up some lengths:
     * n = number of cases
     * J = number of judges
     */
    uint n = Y.rows();
    uint J = Y.cols();

    // Pointers/iterators to results arrays
    double* sigmasq_cur = sigmasq_store;
    double* gamma_cur = gamma_store;
    double* z_cur = 0;
    double* t_cur = 0;

    if (save_traits)
      z_cur = z_store;
    if (save_perceived_traits)
      t_cur = t_store;

    /* Build a binary matrix of indicating which observations judges
     * graded (n x J)
     *
     * XXX maybe use a hash for this to save mem but probably not an
     * issue.
     */
    bmatrix did_grade = Y >= 1; 

    /* Initialize variables */
    matrix Z(n, 1, false); // Can start out uninitialized
    if (t.isNull())
      t = init_perceived(Y, C);
    if (gamma.isNull())
      gamma = init_gamma(C); // dim is J x (max(C) + 1)
    if (sigmasq.isNull())
      sigmasq = init_sigmasq(J);

    // Disperse starting values
    if (disperse) {
      disperse_t(t, did_grade, myrng, mutex);
      disperse_gamma(gamma, C, myrng, mutex);
      disperse_sigmasq(sigmasq, myrng, mutex);
    }


    /* The Gibbs/Metropolis-Hastings hybrid sample */
    matrix arate;  // The gamma MH acceptance rates
    matrix accepted(1, J); // Numbers of gamma MH acceptances

    uint pb_iter = 0;
    matrix pb_arate; // Post burnin gamma MH acceptance rates
    matrix pb_accepted(1, J); // Post burnin number of acceptances

    for (uint iter = 0; iter <  burnin + mcmc_its; ++iter) {
      /* g(Z | t_ij, sigma^2_j) */
      Z = sample_trait(t, sigmasq, did_grade, mu0, sigmasq0, myrng,
                       mutex);

      /* g(gamma | y, Z) - uses Metropolis Hastings
       * This is exactly like the gamma update step in the basic
       * ordered probit model, except we run the procedure once for
       * each judge's vector of gammas.  This means there are J MH
       * steps and, therefore, J acceptance rates.
       */

      for (uint j = 0; j < J; ++j) {
        matrix gamma_j = scythe::t(gamma(j, 0, j, C[j]));
        matrix gamma_p = 
          propose_gamma(gamma_j, sigma_mh[j], myrng, mutex);

        //if (mutex != NULL) pthread_mutex_lock (mutex);
        double coin = myrng.runif();
        //if (mutex != NULL) pthread_mutex_unlock (mutex);
        if (coin <= 
          accept_ratio(gamma_p, gamma_j, Y(scythe::_, j), Z, 
                       did_grade(scythe::_, j), 
                       std::sqrt(sigmasq[j]), sigma_mh[j])) {
          // Scythe 1.0 submatrix assignment at work!
          //gamma(j, 0, j, C[j]) = gamma_p;
          for (uint c = 0; c < C[j] + 1; ++c)
            gamma(j, c) = gamma_p[c];

          ++accepted[j];
          if (iter >= burnin)
            ++pb_accepted[j];
        }
      }

      /* g(t_ij | Z, gamma, sigma^2_j)
       * Note: there are no estimates of perceived latent traits for
       * responses not graded by a given judge.
       *
       * Also, we calculate n_j and S for the next step here, to save
       * a loop.
       */
      matrix S(J, 1, false);
      imatrix ns(J, 1, false);
      for (uint j = 0; j < J; ++j) {
        ns[j] = 0;
        S[j] = 0;
        for (uint i = 0; i < n; ++i) {
          if (did_grade(i, j)) {
            //std::cout << i << ", " << j << ", " << n << ": "
            //          << gamma(j, Y(i, j) - 1) << " - "
            //          << gamma(j, Y(i, j)) << "\n";
            //if (mutex != NULL) pthread_mutex_lock (mutex);
            double tmp = myrng.rtnorm_combo(Z[i], sigmasq[j],
                                            gamma(j, Y(i, j) - 1),
                                            gamma(j, Y(i, j)));
            //if (mutex != NULL) pthread_mutex_unlock (mutex);
            if (std::isnan(tmp))
              Rprintf("NaN in t(%d, %d)!\n", i, j);
            t(i,j) = tmp;

            ++ns[j];
            S[j] += std::pow((t(i, j) - Z[i]), 2);
          } else
            t(i, j) = NAN;
        }
      }

      //Rprintf("chain %d it %d !4\n", chain, iter);
      /* g(sigma^2_j | Z_i, t_ij) */
      for (uint j = 0; j < J; ++j) {
        //if (mutex != NULL) pthread_mutex_lock (mutex);
        sigmasq[j] 
          = myrng.rigamma(ns[j] / 2 + alpha, S[j] / 2 + lambda);
        //if (mutex != NULL) pthread_mutex_unlock (mutex);
      }

      /* Store results */
      if (iter >= burnin && (iter + 1) % thin == 0) {
        sigmasq_cur = std::copy(sigmasq.begin(),sigmasq.end(),sigmasq_cur);
        gamma_cur = std::copy(gamma.begin(), gamma.end(), gamma_cur);
        if (save_traits)
          z_cur = std::copy(Z.begin(), Z.end(), z_cur);
        if (save_perceived_traits)
          t_cur = std::copy(t.template begin<scythe::Col>(), 
              t.template end<scythe::Col>(), t_cur);
      }
      
      /* Update acceptance rate */
      arate = accepted / (iter + 1);

      /* And post-burnin accpetance rate */
      if (iter >= burnin)
        pb_arate = pb_accepted / (++pb_iter);

      /* Print info */
      if (verbose > 0 && (iter + 1) % verbose == 0) {
        Rprintf("Chain %d Multi-rater ordered probit iteration %d\n", 
            chain, (iter+1));
        Rprintf("MH sigma =");
        for (unsigned int j = 0; j < sigma_mh.size(); ++j)
          Rprintf(" %f", sigma_mh(j));
        Rprintf("\nMetropolis-Hastings step acceptance rates:");
        for (unsigned int j = 0; j < arate.size(); ++j)
          Rprintf(" %f", arate(j));
        if (iter >= burnin) {
          Rprintf("\nPost Burnin Metropolis-Hastings step acceptance rates:\n");
          for (unsigned int j = 0; j < pb_arate.size(); ++j)
            Rprintf(" %f", pb_arate(j));
        }
        Rprintf("\n\n");
      }

      //R_CheckUserInterrupt(); // let ctrl-C break us out of loop
    }
          
    Rprintf("Chain %d Final Metropolis-Hastings step acceptance rate = ",
        chain);
    for (unsigned int j = 0; j < arate.size(); ++j)
      Rprintf(" %f", arate(j));
    Rprintf("\n\n");

    //if (mutex != NULL) pthread_mutex_unlock (mutex);
    return 0;
  }
}
