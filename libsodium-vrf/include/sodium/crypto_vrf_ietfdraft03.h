#ifndef crypto_vrf_ietfdraft03_H
#define crypto_vrf_ietfdraft03_H

#include <stddef.h>

#include "export.h"

#ifdef __cplusplus
# ifdef __GNUC__
#  pragma GCC diagnostic ignored "-Wlong-long"
# endif
extern "C" {
#endif

#define crypto_vrf_ietfdraft03_BYTES 80U
SODIUM_EXPORT
size_t crypto_vrf_ietfdraft03_bytes(void);

SODIUM_EXPORT
size_t crypto_vrf_ietfdraft03_proofbytes(void); // Kept for backwards compatibility

#define crypto_vrf_ietfdraft03_OUTPUTBYTES 64U
SODIUM_EXPORT
size_t crypto_vrf_ietfdraft03_outputbytes(void);

#define crypto_vrf_ietfdraft03_SEEDBYTES 32U
SODIUM_EXPORT
size_t crypto_vrf_ietfdraft03_seedbytes(void);

#define crypto_vrf_ietfdraft03_PUBLICKEYBYTES 32U
SODIUM_EXPORT
size_t crypto_vrf_ietfdraft03_publickeybytes(void);

#define crypto_vrf_ietfdraft03_SECRETKEYBYTES 64U
SODIUM_EXPORT
size_t crypto_vrf_ietfdraft03_secretkeybytes(void);

SODIUM_EXPORT
int crypto_vrf_ietfdraft03_prove(unsigned char *proof,
                                 const unsigned char *sk,
                                 const unsigned char *m,
                                 unsigned long long mlen);

SODIUM_EXPORT
int crypto_vrf_ietfdraft03_verify(unsigned char *output,
                                  const unsigned char *pk,
                                  const unsigned char *proof,
                                  const unsigned char *m,
                                  unsigned long long mlen)
__attribute__ ((warn_unused_result)) __attribute__ ((nonnull));

SODIUM_EXPORT
int crypto_vrf_ietfdraft03_proof_to_hash(unsigned char *hash,
                                         const unsigned char *proof)
__attribute__ ((nonnull));

/*
 * We keep the functions below to be backwards compatible with older
 * versions of the cardano node, but these are identical as those
 * without the versioning in crypto_vrf.h
 */

SODIUM_EXPORT
int crypto_vrf_ietfdraft03_keypair_from_seed(unsigned char *pk, unsigned char *sk,
                            const unsigned char *seed)
__attribute__ ((nonnull));

SODIUM_EXPORT
void crypto_vrf_ietfdraft03_sk_to_pk(unsigned char *pk,
                         const unsigned char *skpk);

SODIUM_EXPORT
void crypto_vrf_ietfdraft03_sk_to_seed(unsigned char *seed,
                           const unsigned char *skpk);

#ifdef __cplusplus
}
#endif

#endif