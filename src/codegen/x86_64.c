#include "xed/xed-interface.h"
#include <stddef.h>

// xed_encoder_instruction_t has a bitfield in a union, which Zig won't touch
// with a 10ft pole, so we have to do this in C
_Bool _codegen_xed_build(xed_encoder_request_t *req, xed_iclass_enum_t iclass,
                         xed_uint_t operand_width, xed_state_t mode,
                         size_t noperand,
                         const xed_encoder_operand_t *operands) {
  xed_encoder_instruction_t inst;
  xed_inst(&inst, mode, iclass, operand_width, noperand, operands);
  return xed_convert_to_encoder_request(req, &inst);
}
