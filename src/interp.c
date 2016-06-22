#include <RustyScheme.h>

#define OPCODES(OPCODE) \
   OPCODE(cons) OPCODE(car) OPCODE(cdr) OPCODE(aref) OPCODE(aset) \
   OPCODE(throw) OPCODE(make_array) OPCODE(apply) OPCODE(call) \
   OPCODE(tailcall) OPCODE(go) OPCODE(catch) OPCODE(return) \
   OPCODE(setcar) OPCODE(setcdr) OPCODE(setfield)

enum OPCODES {
#define OPCODE(x) OPCODE_##x,
   OPCODES(OPCODE)
#undef OPCODE
}

#if __GNUC__ >= 3
__attribute__((visibility("hidden")))
#endif
void RustyScheme_execute_opcodes(scheme_State *s) {
   const instruction *pc = s->pc;
   value *const registers = s->registers;
#ifdef HAS_COMPUTED_GOTO
# define OP(x) LABEL_##x:
# define DISPATCH(pc) do { goto labels[(pc++)->opcode]; } while (0)
   static const void * labels[const] = {
# define OPCODE(x) &&LABEL_##x,
      OPCODES(OPCODE)
# undef OPCODE
   };
#else
# define SWITCH_CASE(x) case LABEL_##x: goto GOTO_LABEL_##x;
# define OP(x) GOTO_LABEL_##x:
# define DISPATCH(pc) \
   do { \
      switch ((pc++)->opcode) { \
         OPCODES(SWITCH_CASE) \
         default: __builtin_unreachable(); \
      } \
   } while (0)
   OP(cons):
      uint16_t index1 = pc->op_1;
      uint16_t index2 = pc->op_2;
      uint16_t index3 = pc->op_3;
      value new_cons = allocate_cons(registers[index1], registers[index2]);
      registers[index3] = new_cons;
      DISPATCH(pc)
   OP(setcdr):

#endif

