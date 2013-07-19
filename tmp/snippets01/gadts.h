#ifdef GADT_WITNESSES

#define C(contexts) contexts
#define FORALL(types) forall types.
#define PATCHKIND (* -> * -> *)
#define SEALEDPATCHKIND (* -> *)

#else

#define C(contexts)
#define FORALL(types)
#define PATCHKIND *
#define SEALEDPATCHKIND *

#endif
