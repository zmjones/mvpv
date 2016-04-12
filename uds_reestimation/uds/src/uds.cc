#include "matrix.h"
#include "stat.h"
#include "la.h"
#include "rng.h"
#include "mersenne.h"
#include "lecuyer.h"
#include "mroprobit.h"

#include <pthread.h>
#include <R.h>

// for getting rid of stack limit checks
#define CSTACK_DEFNS 1 
#include <Rinterface.h>
#include <stdint.h>

using namespace std;
using namespace scythe;

typedef Matrix<double,Row> matrix;
typedef Matrix<unsigned int,Row> imatrix;

imatrix calc_cats(const imatrix& Y) {
	return(t(maxc(Y)));
}

struct chain_args {
  int chain;
  double* sigmasq_store;
  double* gamma_store;
  double* z_store;
  double* t_store;
  imatrix Y;
  imatrix C;
  scythe::lecuyer myrng;
  double alpha;
  double lambda;
  matrix mu0;
  matrix sigmasq0;
  bool save_traits;
  bool save_perceived_traits;
  matrix sm;
  unsigned int mcmc;
  unsigned int burnin;
  int thin;
  int verbose;
  matrix t_start;
  matrix gamma_start;
  matrix sigmasq_start;
  bool disperse;
  int* failed;
  pthread_mutex_t* mutex;
};

void* mroprobit_chains (void* thread_arg) {
  chain_args* args = (chain_args*) thread_arg;

  *(args->failed) = MROPROBIT::mroprobit(args->chain, 
      args->mutex, args->sigmasq_store,
      args->gamma_store, args->z_store, args->t_store, args->Y,
      args->C, args->myrng, args->alpha, args->lambda, args->mu0,
      args->sigmasq0, args->save_traits, args->save_perceived_traits,
      args->sm, args->mcmc, args->burnin, args->thin, args->verbose,
      args->t_start, args->gamma_start, args->sigmasq_start,
      args->disperse);
 }

extern "C" {

  void mrop (int* n, int* J, int* _Y, double* _sigma_mh,
             double* alpha, double* lambda, double* _mu0,
             int* mu0_long, double* _sigmasq0, int* sigmasq0_long,
             double* _sigmasq_start, int* have_sigmasq_start,
             double* _gamma_start, int* have_gamma_start,
             double* _t_start, int* have_t_start, int* chains, 
             int* _disperse, int* save_traits, int*
             save_perceived_traits, int* mcmc, int* burnin, int* thin,
             int* verbose, int* seed, double* sigmasq_store, double*
             gamma_store, double* z_store, double* t_store, int* failed)
  {
    // Not sure if this is safe, but necessary for pthreads
    R_CStackLimit = (uintptr_t)-1;

    // Setup the mutex (only gets used if *chains > 1)
    pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

    // Setup data
    imatrix Y(*n, *J, _Y);
    imatrix C = calc_cats(Y);
  
    // Setup random number generation
    scythe::mersenne myrng;
    myrng.initialize(*seed);

    // Setup z priors
    matrix mu0;
    if (*mu0_long == 0)
      mu0 = matrix(*n, 1, true, *_mu0);
    else
      mu0 = matrix(*n, 1, _mu0);
    matrix sigmasq0;
    if (*sigmasq0_long == 0)
      sigmasq0 = matrix(*n, 1, true, *_sigmasq0);
    else
      sigmasq0 = matrix(*n, 1, _sigmasq0);

    // Setup m-h tuning params
    matrix sm(1, *J, _sigma_mh);

    // Setup start values
    matrix sigmasq_start;
    if (*have_sigmasq_start == 1)
      sigmasq_start = matrix(*J, 1, _sigmasq_start);
    matrix gamma_start;
    if (*have_gamma_start == 1)
      gamma_start = matrix(*J, max(C) + 1, _gamma_start);
    matrix t_start;
    if (*have_t_start == 1)
      t_start = matrix(*n, *J, _t_start);

    bool disperse = false;
    if (*_disperse != 0)
      disperse = true;

    if (*chains == 1) {
      // run the sampler
      *failed = MROPROBIT::mroprobit(1, NULL, sigmasq_store, gamma_store,
          z_store, t_store, Y, C, myrng, *alpha, *lambda, mu0,
          sigmasq0, *save_traits, *save_perceived_traits, sm, *mcmc,
          *burnin, *thin, *verbose, t_start, gamma_start,
          sigmasq_start, disperse);
    } else {
      // Handle multiple chains/threads
      
      // set the lecuyer seed
      unsigned long lseed[6];
      for (int i = 0; i < 6; ++i)
        lseed[i] = *seed;
      scythe::lecuyer::SetPackageSeed(lseed);

      // pthread setup
      pthread_t* threads = new pthread_t[*chains];
      chain_args* chain = new chain_args[*chains];
      int rc;

      for (int i = 0; i < *chains; ++i) {
        chain[i].chain = i + 1;
        chain[i].sigmasq_store = sigmasq_store;
        sigmasq_store += (*mcmc / *thin) * *J;
        chain[i].gamma_store = gamma_store;
        gamma_store += (*mcmc / *thin) * (*J * (max(C) + 1));
        chain[i].z_store = z_store;
        if (*save_traits != 0)
          z_store += (*mcmc / *thin) * *n;
        chain[i].t_store = t_store;
        if (*save_perceived_traits != 0)
          t_store += (*mcmc / *thin) * *n * *J;
        chain[i].Y = Y;
        chain[i].C = C;
        chain[i].myrng = scythe::lecuyer();
        chain[i].alpha = *alpha;
        chain[i].lambda = *lambda;
        chain[i].mu0 = mu0;
        chain[i].sigmasq0 = sigmasq0;
        chain[i].save_traits = *save_traits;
        chain[i].save_perceived_traits = *save_perceived_traits;
        chain[i].sm = sm;
        chain[i].mcmc = *mcmc;
        chain[i].burnin = *burnin;
        chain[i].thin = *thin;
        chain[i].verbose = *verbose;
        if (*have_t_start == 1)
          chain[i].t_start = t_start;
        if (*have_gamma_start == 1)
          chain[i].gamma_start = gamma_start;
        if (*have_sigmasq_start == 1)
          chain[i].sigmasq_start = sigmasq_start;
        chain[i].disperse = disperse;
        chain[i].failed = failed+i;
        chain[i].mutex = &mutex;
  
        rc = pthread_create(&threads[i], NULL, mroprobit_chains,
                            (void *) (&chain[i]));
        if (rc)
          Rprintf("Chain %d returned with p-thread error code %d\n", i, rc);
      }

      for (int i = 0; i < *chains; ++i)
        pthread_join(threads[i], NULL);

      Rprintf("Completed all chains\n");
      delete[] threads;
      delete[] chain;
    }
  }


/*
  void ppred (int* _n, int* _J, int* _S, double* _z, double* _gamma, 
              double* _sigmasq, double* _Kj, int* _gamma_index,
              double* ystar)
  {
    int n = *_n;
    int J = *_J;
    int S = *_S;

    Matrix<> z(S, n, _z);
    Matrix<> Kj(J, 1, _Kj);
    Matrix<> gamma(S, scythe::max(Kj), _gamma);
    Matrix<> sigmasq(S, J, _sigmasq);
    Matrix<int> gamma_index(2, J, _gamma_index);


    
  }
*/
 
}
