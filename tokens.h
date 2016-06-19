#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: bpmn.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define STARTP 2
#define ENDP 3
#define CONN 4
#define FILECONN 5
#define CRIT 6
#define DIFFER 7
#define CORRECTF 8
#define FILEREAD 9
#define FILEWRITE 10
#define OPENP 11
#define CLOSEP 12
#define QUERIES 13
#define GPAR 14
#define GOR 15
#define GXOR 16
#define SEQ 17
#define ID 18
#define SPACE 19

#ifdef __USE_PROTOS
void bpmn(void);
#else
extern void bpmn();
#endif

#ifdef __USE_PROTOS
void process(void);
#else
extern void process();
#endif

#ifdef __USE_PROTOS
void start(void);
#else
extern void start();
#endif

#ifdef __USE_PROTOS
void operations(void);
#else
extern void operations();
#endif

#ifdef __USE_PROTOS
void conex(void);
#else
extern void conex();
#endif

#ifdef __USE_PROTOS
void file(void);
#else
extern void file();
#endif

#ifdef __USE_PROTOS
void queries(void);
#else
extern void queries();
#endif

#ifdef __USE_PROTOS
void crit(void);
#else
extern void crit();
#endif

#ifdef __USE_PROTOS
void dif(void);
#else
extern void dif();
#endif

#ifdef __USE_PROTOS
void correctf(void);
#else
extern void correctf();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType setwd1[];
extern SetWordType setwd2[];
