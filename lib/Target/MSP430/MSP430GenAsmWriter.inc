/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Assembly Writer Source Fragment                                            *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

/// printInstruction - This method is automatically generated by tablegen
/// from the instruction set description.
void MSP430InstPrinter::printInstruction(const MCInst *MI, raw_ostream &O) {
  static const char AsmStrs[] = {
  /* 0 */ 'r', 'r', 'a', 9, 0,
  /* 5 */ 'r', 'r', 'a', '.', 'b', 9, 0,
  /* 12 */ 's', 'u', 'b', '.', 'b', 9, 0,
  /* 19 */ 's', 'u', 'b', 'c', '.', 'b', 9, 0,
  /* 27 */ 'a', 'd', 'd', 'c', '.', 'b', 9, 0,
  /* 35 */ 'b', 'i', 'c', '.', 'b', 9, 0,
  /* 42 */ 'r', 'r', 'c', '.', 'b', 9, 0,
  /* 49 */ 'd', 'a', 'd', 'd', '.', 'b', 9, 0,
  /* 57 */ 'a', 'n', 'd', '.', 'b', 9, 0,
  /* 64 */ 'p', 'u', 's', 'h', '.', 'b', 9, 0,
  /* 72 */ 'c', 'm', 'p', '.', 'b', 9, 0,
  /* 79 */ 'x', 'o', 'r', '.', 'b', 9, 0,
  /* 86 */ 'b', 'i', 's', '.', 'b', 9, 0,
  /* 93 */ 'b', 'i', 't', '.', 'b', 9, 0,
  /* 100 */ 'm', 'o', 'v', '.', 'b', 9, 0,
  /* 107 */ 's', 'w', 'p', 'b', 9, 0,
  /* 113 */ 's', 'u', 'b', 9, 0,
  /* 118 */ 's', 'u', 'b', 'c', 9, 0,
  /* 124 */ 'a', 'd', 'd', 'c', 9, 0,
  /* 130 */ 'b', 'i', 'c', 9, 0,
  /* 135 */ 'r', 'r', 'c', 9, 0,
  /* 140 */ 'd', 'a', 'd', 'd', 9, 0,
  /* 146 */ 'a', 'n', 'd', 9, 0,
  /* 151 */ 'p', 'u', 's', 'h', 9, 0,
  /* 157 */ 'c', 'a', 'l', 'l', 9, 0,
  /* 163 */ 'c', 'm', 'p', 9, 0,
  /* 168 */ 'j', 'm', 'p', 9, 0,
  /* 173 */ 'p', 'o', 'p', 9, 0,
  /* 178 */ 'b', 'r', 9, 0,
  /* 182 */ 'x', 'o', 'r', 9, 0,
  /* 187 */ 'b', 'i', 's', 9, 0,
  /* 192 */ 'b', 'i', 't', 9, 0,
  /* 197 */ 's', 'x', 't', 9, 0,
  /* 202 */ 'm', 'o', 'v', 9, 0,
  /* 207 */ '#', 'A', 'D', 'J', 'C', 'A', 'L', 'L', 'S', 'T', 'A', 'C', 'K', 'D', 'O', 'W', 'N', 32, 0,
  /* 226 */ '#', 'A', 'D', 'J', 'C', 'A', 'L', 'L', 'S', 'T', 'A', 'C', 'K', 'U', 'P', 32, 0,
  /* 243 */ '#', 32, 'X', 'R', 'a', 'y', 32, 'F', 'u', 'n', 'c', 't', 'i', 'o', 'n', 32, 'P', 'a', 't', 'c', 'h', 'a', 'b', 'l', 'e', 32, 'R', 'E', 'T', '.', 0,
  /* 274 */ '#', 32, 'X', 'R', 'a', 'y', 32, 'T', 'y', 'p', 'e', 'd', 32, 'E', 'v', 'e', 'n', 't', 32, 'L', 'o', 'g', '.', 0,
  /* 298 */ '#', 32, 'X', 'R', 'a', 'y', 32, 'C', 'u', 's', 't', 'o', 'm', 32, 'E', 'v', 'e', 'n', 't', 32, 'L', 'o', 'g', '.', 0,
  /* 323 */ '#', 32, 'X', 'R', 'a', 'y', 32, 'F', 'u', 'n', 'c', 't', 'i', 'o', 'n', 32, 'E', 'n', 't', 'e', 'r', '.', 0,
  /* 346 */ '#', 32, 'X', 'R', 'a', 'y', 32, 'T', 'a', 'i', 'l', 32, 'C', 'a', 'l', 'l', 32, 'E', 'x', 'i', 't', '.', 0,
  /* 369 */ '#', 32, 'X', 'R', 'a', 'y', 32, 'F', 'u', 'n', 'c', 't', 'i', 'o', 'n', 32, 'E', 'x', 'i', 't', '.', 0,
  /* 391 */ 'L', 'I', 'F', 'E', 'T', 'I', 'M', 'E', '_', 'E', 'N', 'D', 0,
  /* 404 */ 'B', 'U', 'N', 'D', 'L', 'E', 0,
  /* 411 */ 'D', 'B', 'G', '_', 'V', 'A', 'L', 'U', 'E', 0,
  /* 421 */ 'D', 'B', 'G', '_', 'L', 'A', 'B', 'E', 'L', 0,
  /* 431 */ '#', 32, 'S', 'r', 'a', '1', '6', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 446 */ '#', 32, 'S', 'h', 'l', '1', '6', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 461 */ '#', 32, 'S', 'r', 'l', '1', '6', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 476 */ '#', 32, 'S', 'e', 'l', 'e', 'c', 't', '1', '6', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 494 */ '#', 32, 'S', 'r', 'a', '8', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 508 */ '#', 32, 'S', 'h', 'l', '8', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 522 */ '#', 32, 'S', 'r', 'l', '8', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 536 */ '#', 32, 'S', 'e', 'l', 'e', 'c', 't', '8', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 553 */ '#', 32, 'A', 'D', 'D', 'f', 'r', 'a', 'm', 'e', 32, 'P', 'S', 'E', 'U', 'D', 'O', 0,
  /* 571 */ 'L', 'I', 'F', 'E', 'T', 'I', 'M', 'E', '_', 'S', 'T', 'A', 'R', 'T', 0,
  /* 586 */ 'r', 'e', 't', 'i', 0,
  /* 591 */ 'j', 0,
  /* 593 */ '#', 32, 'F', 'E', 'n', 't', 'r', 'y', 32, 'c', 'a', 'l', 'l', 0,
  /* 607 */ 'r', 'e', 't', 0,
  };

  static const uint16_t OpInfo0[] = {
    0U,	// PHI
    0U,	// INLINEASM
    0U,	// INLINEASM_BR
    0U,	// CFI_INSTRUCTION
    0U,	// EH_LABEL
    0U,	// GC_LABEL
    0U,	// ANNOTATION_LABEL
    0U,	// KILL
    0U,	// EXTRACT_SUBREG
    0U,	// INSERT_SUBREG
    0U,	// IMPLICIT_DEF
    0U,	// SUBREG_TO_REG
    0U,	// COPY_TO_REGCLASS
    412U,	// DBG_VALUE
    422U,	// DBG_LABEL
    0U,	// REG_SEQUENCE
    0U,	// COPY
    405U,	// BUNDLE
    572U,	// LIFETIME_START
    392U,	// LIFETIME_END
    0U,	// STACKMAP
    594U,	// FENTRY_CALL
    0U,	// PATCHPOINT
    0U,	// LOAD_STACK_GUARD
    0U,	// STATEPOINT
    0U,	// LOCAL_ESCAPE
    0U,	// FAULTING_OP
    0U,	// PATCHABLE_OP
    324U,	// PATCHABLE_FUNCTION_ENTER
    244U,	// PATCHABLE_RET
    370U,	// PATCHABLE_FUNCTION_EXIT
    347U,	// PATCHABLE_TAIL_CALL
    299U,	// PATCHABLE_EVENT_CALL
    275U,	// PATCHABLE_TYPED_EVENT_CALL
    0U,	// ICALL_BRANCH_FUNNEL
    0U,	// G_ADD
    0U,	// G_SUB
    0U,	// G_MUL
    0U,	// G_SDIV
    0U,	// G_UDIV
    0U,	// G_SREM
    0U,	// G_UREM
    0U,	// G_AND
    0U,	// G_OR
    0U,	// G_XOR
    0U,	// G_IMPLICIT_DEF
    0U,	// G_PHI
    0U,	// G_FRAME_INDEX
    0U,	// G_GLOBAL_VALUE
    0U,	// G_EXTRACT
    0U,	// G_UNMERGE_VALUES
    0U,	// G_INSERT
    0U,	// G_MERGE_VALUES
    0U,	// G_BUILD_VECTOR
    0U,	// G_BUILD_VECTOR_TRUNC
    0U,	// G_CONCAT_VECTORS
    0U,	// G_PTRTOINT
    0U,	// G_INTTOPTR
    0U,	// G_BITCAST
    0U,	// G_INTRINSIC_TRUNC
    0U,	// G_INTRINSIC_ROUND
    0U,	// G_LOAD
    0U,	// G_SEXTLOAD
    0U,	// G_ZEXTLOAD
    0U,	// G_STORE
    0U,	// G_ATOMIC_CMPXCHG_WITH_SUCCESS
    0U,	// G_ATOMIC_CMPXCHG
    0U,	// G_ATOMICRMW_XCHG
    0U,	// G_ATOMICRMW_ADD
    0U,	// G_ATOMICRMW_SUB
    0U,	// G_ATOMICRMW_AND
    0U,	// G_ATOMICRMW_NAND
    0U,	// G_ATOMICRMW_OR
    0U,	// G_ATOMICRMW_XOR
    0U,	// G_ATOMICRMW_MAX
    0U,	// G_ATOMICRMW_MIN
    0U,	// G_ATOMICRMW_UMAX
    0U,	// G_ATOMICRMW_UMIN
    0U,	// G_FENCE
    0U,	// G_BRCOND
    0U,	// G_BRINDIRECT
    0U,	// G_INTRINSIC
    0U,	// G_INTRINSIC_W_SIDE_EFFECTS
    0U,	// G_ANYEXT
    0U,	// G_TRUNC
    0U,	// G_CONSTANT
    0U,	// G_FCONSTANT
    0U,	// G_VASTART
    0U,	// G_VAARG
    0U,	// G_SEXT
    0U,	// G_ZEXT
    0U,	// G_SHL
    0U,	// G_LSHR
    0U,	// G_ASHR
    0U,	// G_ICMP
    0U,	// G_FCMP
    0U,	// G_SELECT
    0U,	// G_UADDO
    0U,	// G_UADDE
    0U,	// G_USUBO
    0U,	// G_USUBE
    0U,	// G_SADDO
    0U,	// G_SADDE
    0U,	// G_SSUBO
    0U,	// G_SSUBE
    0U,	// G_UMULO
    0U,	// G_SMULO
    0U,	// G_UMULH
    0U,	// G_SMULH
    0U,	// G_FADD
    0U,	// G_FSUB
    0U,	// G_FMUL
    0U,	// G_FMA
    0U,	// G_FDIV
    0U,	// G_FREM
    0U,	// G_FPOW
    0U,	// G_FEXP
    0U,	// G_FEXP2
    0U,	// G_FLOG
    0U,	// G_FLOG2
    0U,	// G_FLOG10
    0U,	// G_FNEG
    0U,	// G_FPEXT
    0U,	// G_FPTRUNC
    0U,	// G_FPTOSI
    0U,	// G_FPTOUI
    0U,	// G_SITOFP
    0U,	// G_UITOFP
    0U,	// G_FABS
    0U,	// G_FCOPYSIGN
    0U,	// G_FCANONICALIZE
    0U,	// G_FMINNUM
    0U,	// G_FMAXNUM
    0U,	// G_FMINNUM_IEEE
    0U,	// G_FMAXNUM_IEEE
    0U,	// G_FMINIMUM
    0U,	// G_FMAXIMUM
    0U,	// G_GEP
    0U,	// G_PTR_MASK
    0U,	// G_SMIN
    0U,	// G_SMAX
    0U,	// G_UMIN
    0U,	// G_UMAX
    0U,	// G_BR
    0U,	// G_BRJT
    0U,	// G_INSERT_VECTOR_ELT
    0U,	// G_EXTRACT_VECTOR_ELT
    0U,	// G_SHUFFLE_VECTOR
    0U,	// G_CTTZ
    0U,	// G_CTTZ_ZERO_UNDEF
    0U,	// G_CTLZ
    0U,	// G_CTLZ_ZERO_UNDEF
    0U,	// G_CTPOP
    0U,	// G_BSWAP
    0U,	// G_FCEIL
    0U,	// G_FCOS
    0U,	// G_FSIN
    0U,	// G_FSQRT
    0U,	// G_FFLOOR
    0U,	// G_FRINT
    0U,	// G_FNEARBYINT
    0U,	// G_ADDRSPACE_CAST
    0U,	// G_BLOCK_ADDR
    0U,	// G_JUMP_TABLE
    1166U,	// ADD16mc
    1166U,	// ADD16mi
    2190U,	// ADD16mm
    3214U,	// ADD16mn
    4238U,	// ADD16mp
    1166U,	// ADD16mr
    17550U,	// ADD16rc
    17550U,	// ADD16ri
    18574U,	// ADD16rm
    19598U,	// ADD16rn
    5262U,	// ADD16rp
    17550U,	// ADD16rr
    1075U,	// ADD8mc
    1075U,	// ADD8mi
    2099U,	// ADD8mm
    3123U,	// ADD8mn
    4147U,	// ADD8mp
    1075U,	// ADD8mr
    17459U,	// ADD8rc
    17459U,	// ADD8ri
    18483U,	// ADD8rm
    19507U,	// ADD8rn
    5171U,	// ADD8rp
    17459U,	// ADD8rr
    1149U,	// ADDC16mc
    1149U,	// ADDC16mi
    2173U,	// ADDC16mm
    3197U,	// ADDC16mn
    4221U,	// ADDC16mp
    1149U,	// ADDC16mr
    17533U,	// ADDC16rc
    17533U,	// ADDC16ri
    18557U,	// ADDC16rm
    19581U,	// ADDC16rn
    5245U,	// ADDC16rp
    17533U,	// ADDC16rr
    1052U,	// ADDC8mc
    1052U,	// ADDC8mi
    2076U,	// ADDC8mm
    3100U,	// ADDC8mn
    4124U,	// ADDC8mp
    1052U,	// ADDC8mr
    17436U,	// ADDC8rc
    17436U,	// ADDC8ri
    18460U,	// ADDC8rm
    19484U,	// ADDC8rn
    5148U,	// ADDC8rp
    17436U,	// ADDC8rr
    554U,	// ADDframe
    39120U,	// ADJCALLSTACKDOWN
    39139U,	// ADJCALLSTACKUP
    1171U,	// AND16mc
    1171U,	// AND16mi
    2195U,	// AND16mm
    3219U,	// AND16mn
    4243U,	// AND16mp
    1171U,	// AND16mr
    17555U,	// AND16rc
    17555U,	// AND16ri
    18579U,	// AND16rm
    19603U,	// AND16rn
    5267U,	// AND16rp
    17555U,	// AND16rr
    1082U,	// AND8mc
    1082U,	// AND8mi
    2106U,	// AND8mm
    3130U,	// AND8mn
    4154U,	// AND8mp
    1082U,	// AND8mr
    17466U,	// AND8rc
    17466U,	// AND8ri
    18490U,	// AND8rm
    19514U,	// AND8rn
    5178U,	// AND8rp
    17466U,	// AND8rr
    1155U,	// BIC16mc
    1155U,	// BIC16mi
    2179U,	// BIC16mm
    3203U,	// BIC16mn
    4227U,	// BIC16mp
    1155U,	// BIC16mr
    17539U,	// BIC16rc
    17539U,	// BIC16ri
    18563U,	// BIC16rm
    19587U,	// BIC16rn
    5251U,	// BIC16rp
    17539U,	// BIC16rr
    1060U,	// BIC8mc
    1060U,	// BIC8mi
    2084U,	// BIC8mm
    3108U,	// BIC8mn
    4132U,	// BIC8mp
    1060U,	// BIC8mr
    17444U,	// BIC8rc
    17444U,	// BIC8ri
    18468U,	// BIC8rm
    19492U,	// BIC8rn
    5156U,	// BIC8rp
    17444U,	// BIC8rr
    1212U,	// BIS16mc
    1212U,	// BIS16mi
    2236U,	// BIS16mm
    3260U,	// BIS16mn
    4284U,	// BIS16mp
    1212U,	// BIS16mr
    17596U,	// BIS16rc
    17596U,	// BIS16ri
    18620U,	// BIS16rm
    19644U,	// BIS16rn
    5308U,	// BIS16rp
    17596U,	// BIS16rr
    1111U,	// BIS8mc
    1111U,	// BIS8mi
    2135U,	// BIS8mm
    3159U,	// BIS8mn
    4183U,	// BIS8mp
    1111U,	// BIS8mr
    17495U,	// BIS8rc
    17495U,	// BIS8ri
    18519U,	// BIS8rm
    19543U,	// BIS8rn
    5207U,	// BIS8rp
    17495U,	// BIS8rr
    1217U,	// BIT16mc
    1217U,	// BIT16mi
    2241U,	// BIT16mm
    3265U,	// BIT16mn
    4289U,	// BIT16mp
    1217U,	// BIT16mr
    7361U,	// BIT16rc
    7361U,	// BIT16ri
    8385U,	// BIT16rm
    9409U,	// BIT16rn
    10433U,	// BIT16rp
    7361U,	// BIT16rr
    1118U,	// BIT8mc
    1118U,	// BIT8mi
    2142U,	// BIT8mm
    3166U,	// BIT8mn
    4190U,	// BIT8mp
    1118U,	// BIT8mr
    7262U,	// BIT8rc
    7262U,	// BIT8ri
    8286U,	// BIT8rm
    9310U,	// BIT8rn
    10334U,	// BIT8rp
    7262U,	// BIT8rr
    55475U,	// Bi
    11443U,	// Bm
    55475U,	// Br
    55454U,	// CALLi
    11422U,	// CALLm
    12446U,	// CALLn
    13470U,	// CALLp
    55454U,	// CALLr
    1188U,	// CMP16mc
    1188U,	// CMP16mi
    2212U,	// CMP16mm
    3236U,	// CMP16mn
    4260U,	// CMP16mp
    1188U,	// CMP16mr
    7332U,	// CMP16rc
    7332U,	// CMP16ri
    8356U,	// CMP16rm
    9380U,	// CMP16rn
    10404U,	// CMP16rp
    7332U,	// CMP16rr
    1097U,	// CMP8mc
    1097U,	// CMP8mi
    2121U,	// CMP8mm
    3145U,	// CMP8mn
    4169U,	// CMP8mp
    1097U,	// CMP8mr
    7241U,	// CMP8rc
    7241U,	// CMP8ri
    8265U,	// CMP8rm
    9289U,	// CMP8rn
    10313U,	// CMP8rp
    7241U,	// CMP8rr
    1165U,	// DADD16mc
    1165U,	// DADD16mi
    2189U,	// DADD16mm
    3213U,	// DADD16mn
    4237U,	// DADD16mp
    1165U,	// DADD16mr
    17549U,	// DADD16rc
    17549U,	// DADD16ri
    18573U,	// DADD16rm
    19597U,	// DADD16rn
    5261U,	// DADD16rp
    17549U,	// DADD16rr
    1074U,	// DADD8mc
    1074U,	// DADD8mi
    2098U,	// DADD8mm
    3122U,	// DADD8mn
    4146U,	// DADD8mp
    1074U,	// DADD8mr
    17458U,	// DADD8rc
    17458U,	// DADD8ri
    18482U,	// DADD8rm
    19506U,	// DADD8rn
    5170U,	// DADD8rp
    17458U,	// DADD8rr
    14928U,	// JCC
    15529U,	// JMP
    1227U,	// MOV16mc
    1227U,	// MOV16mi
    2251U,	// MOV16mm
    3275U,	// MOV16mn
    1227U,	// MOV16mr
    7371U,	// MOV16rc
    7371U,	// MOV16ri
    8395U,	// MOV16rm
    9419U,	// MOV16rn
    20683U,	// MOV16rp
    7371U,	// MOV16rr
    1125U,	// MOV8mc
    1125U,	// MOV8mi
    2149U,	// MOV8mm
    3173U,	// MOV8mn
    1125U,	// MOV8mr
    7269U,	// MOV8rc
    7269U,	// MOV8ri
    8293U,	// MOV8rm
    9317U,	// MOV8rn
    20581U,	// MOV8rp
    7269U,	// MOV8rr
    8293U,	// MOVZX16rm8
    7269U,	// MOVZX16rr8
    55470U,	// POP16r
    55448U,	// PUSH16c
    55448U,	// PUSH16i
    55448U,	// PUSH16r
    55361U,	// PUSH8r
    608U,	// RET
    587U,	// RETI
    11265U,	// RRA16m
    12289U,	// RRA16n
    13313U,	// RRA16p
    55297U,	// RRA16r
    11270U,	// RRA8m
    12294U,	// RRA8n
    13318U,	// RRA8p
    55302U,	// RRA8r
    11400U,	// RRC16m
    12424U,	// RRC16n
    13448U,	// RRC16p
    55432U,	// RRC16r
    11307U,	// RRC8m
    12331U,	// RRC8n
    13355U,	// RRC8p
    55339U,	// RRC8r
    0U,	// Rrcl16
    0U,	// Rrcl8
    11462U,	// SEXT16m
    12486U,	// SEXT16n
    13510U,	// SEXT16p
    55494U,	// SEXT16r
    1138U,	// SUB16mc
    1138U,	// SUB16mi
    2162U,	// SUB16mm
    3186U,	// SUB16mn
    4210U,	// SUB16mp
    1138U,	// SUB16mr
    17522U,	// SUB16rc
    17522U,	// SUB16ri
    18546U,	// SUB16rm
    19570U,	// SUB16rn
    5234U,	// SUB16rp
    17522U,	// SUB16rr
    1037U,	// SUB8mc
    1037U,	// SUB8mi
    2061U,	// SUB8mm
    3085U,	// SUB8mn
    4109U,	// SUB8mp
    1037U,	// SUB8mr
    17421U,	// SUB8rc
    17421U,	// SUB8ri
    18445U,	// SUB8rm
    19469U,	// SUB8rn
    5133U,	// SUB8rp
    17421U,	// SUB8rr
    1143U,	// SUBC16mc
    1143U,	// SUBC16mi
    2167U,	// SUBC16mm
    3191U,	// SUBC16mn
    4215U,	// SUBC16mp
    1143U,	// SUBC16mr
    17527U,	// SUBC16rc
    17527U,	// SUBC16ri
    18551U,	// SUBC16rm
    19575U,	// SUBC16rn
    5239U,	// SUBC16rp
    17527U,	// SUBC16rr
    1044U,	// SUBC8mc
    1044U,	// SUBC8mi
    2068U,	// SUBC8mm
    3092U,	// SUBC8mn
    4116U,	// SUBC8mp
    1044U,	// SUBC8mr
    17428U,	// SUBC8rc
    17428U,	// SUBC8ri
    18452U,	// SUBC8rm
    19476U,	// SUBC8rn
    5140U,	// SUBC8rp
    17428U,	// SUBC8rr
    11372U,	// SWPB16m
    12396U,	// SWPB16n
    13420U,	// SWPB16p
    55404U,	// SWPB16r
    477U,	// Select16
    537U,	// Select8
    447U,	// Shl16
    509U,	// Shl8
    432U,	// Sra16
    495U,	// Sra8
    462U,	// Srl16
    523U,	// Srl8
    1207U,	// XOR16mc
    1207U,	// XOR16mi
    2231U,	// XOR16mm
    3255U,	// XOR16mn
    4279U,	// XOR16mp
    1207U,	// XOR16mr
    17591U,	// XOR16rc
    17591U,	// XOR16ri
    18615U,	// XOR16rm
    19639U,	// XOR16rn
    5303U,	// XOR16rp
    17591U,	// XOR16rr
    1104U,	// XOR8mc
    1104U,	// XOR8mi
    2128U,	// XOR8mm
    3152U,	// XOR8mn
    4176U,	// XOR8mp
    1104U,	// XOR8mr
    17488U,	// XOR8rc
    17488U,	// XOR8ri
    18512U,	// XOR8rm
    19536U,	// XOR8rn
    5200U,	// XOR8rp
    17488U,	// XOR8rr
    7269U,	// ZEXT16r
  };

  O << "\t";

  // Emit the opcode for the instruction.
  uint32_t Bits = 0;
  Bits |= OpInfo0[MI->getOpcode()] << 0;
  assert(Bits != 0 && "Cannot print this instruction.");
  O << AsmStrs+(Bits & 1023)-1;


  // Fragment 0 encoded into 4 bits for 16 unique commands.
  switch ((Bits >> 10) & 15) {
  default: llvm_unreachable("Invalid command number.");
  case 0:
    // DBG_VALUE, DBG_LABEL, BUNDLE, LIFETIME_START, LIFETIME_END, FENTRY_CAL...
    return;
    break;
  case 1:
    // ADD16mc, ADD16mi, ADD16mr, ADD16rc, ADD16ri, ADD16rr, ADD8mc, ADD8mi, ...
    printOperand(MI, 2, O);
    O << ", ";
    break;
  case 2:
    // ADD16mm, ADD16rm, ADD8mm, ADD8rm, ADDC16mm, ADDC16rm, ADDC8mm, ADDC8rm...
    printSrcMemOperand(MI, 2, O);
    O << ", ";
    break;
  case 3:
    // ADD16mn, ADD16rn, ADD8mn, ADD8rn, ADDC16mn, ADDC16rn, ADDC8mn, ADDC8rn...
    printIndRegOperand(MI, 2, O);
    O << ", ";
    break;
  case 4:
    // ADD16mp, ADD8mp, ADDC16mp, ADDC8mp, AND16mp, AND8mp, BIC16mp, BIC8mp, ...
    printPostIndRegOperand(MI, 2, O);
    O << ", ";
    break;
  case 5:
    // ADD16rp, ADD8rp, ADDC16rp, ADDC8rp, AND16rp, AND8rp, BIC16rp, BIC8rp, ...
    printPostIndRegOperand(MI, 3, O);
    O << ", ";
    printOperand(MI, 0, O);
    return;
    break;
  case 6:
    // ADJCALLSTACKDOWN, ADJCALLSTACKUP, Bi, Br, CALLi, CALLr, POP16r, PUSH16...
    printOperand(MI, 0, O);
    break;
  case 7:
    // BIT16rc, BIT16ri, BIT16rr, BIT8rc, BIT8ri, BIT8rr, CMP16rc, CMP16ri, C...
    printOperand(MI, 1, O);
    O << ", ";
    printOperand(MI, 0, O);
    return;
    break;
  case 8:
    // BIT16rm, BIT8rm, CMP16rm, CMP8rm, MOV16rm, MOV8rm, MOVZX16rm8
    printSrcMemOperand(MI, 1, O);
    O << ", ";
    printOperand(MI, 0, O);
    return;
    break;
  case 9:
    // BIT16rn, BIT8rn, CMP16rn, CMP8rn, MOV16rn, MOV8rn
    printIndRegOperand(MI, 1, O);
    O << ", ";
    printOperand(MI, 0, O);
    return;
    break;
  case 10:
    // BIT16rp, BIT8rp, CMP16rp, CMP8rp
    printPostIndRegOperand(MI, 1, O);
    O << ", ";
    printOperand(MI, 0, O);
    return;
    break;
  case 11:
    // Bm, CALLm, RRA16m, RRA8m, RRC16m, RRC8m, SEXT16m, SWPB16m
    printSrcMemOperand(MI, 0, O);
    return;
    break;
  case 12:
    // CALLn, RRA16n, RRA8n, RRC16n, RRC8n, SEXT16n, SWPB16n
    printIndRegOperand(MI, 0, O);
    return;
    break;
  case 13:
    // CALLp, RRA16p, RRA8p, RRC16p, RRC8p, SEXT16p, SWPB16p
    printPostIndRegOperand(MI, 0, O);
    return;
    break;
  case 14:
    // JCC
    printCCOperand(MI, 1, O);
    O << "\t";
    printPCRelImmOperand(MI, 0, O);
    return;
    break;
  case 15:
    // JMP
    printPCRelImmOperand(MI, 0, O);
    return;
    break;
  }


  // Fragment 1 encoded into 2 bits for 4 unique commands.
  switch ((Bits >> 14) & 3) {
  default: llvm_unreachable("Invalid command number.");
  case 0:
    // ADD16mc, ADD16mi, ADD16mm, ADD16mn, ADD16mp, ADD16mr, ADD8mc, ADD8mi, ...
    printSrcMemOperand(MI, 0, O);
    return;
    break;
  case 1:
    // ADD16rc, ADD16ri, ADD16rm, ADD16rn, ADD16rr, ADD8rc, ADD8ri, ADD8rm, A...
    printOperand(MI, 0, O);
    return;
    break;
  case 2:
    // ADJCALLSTACKDOWN, ADJCALLSTACKUP
    O << ' ';
    printOperand(MI, 1, O);
    return;
    break;
  case 3:
    // Bi, Br, CALLi, CALLr, POP16r, PUSH16c, PUSH16i, PUSH16r, PUSH8r, RRA16...
    return;
    break;
  }

}


/// getRegisterName - This method is automatically generated by tblgen
/// from the register set description.  This returns the assembler name
/// for the specified register.
const char *MSP430InstPrinter::getRegisterName(unsigned RegNo) {
  assert(RegNo && RegNo < 33 && "Invalid register number!");

  static const char AsmStrs[] = {
  /* 0 */ 'r', '1', '0', 0,
  /* 4 */ 'r', '0', 0,
  /* 7 */ 'r', '1', '1', 0,
  /* 11 */ 'r', '1', 0,
  /* 14 */ 'r', '1', '2', 0,
  /* 18 */ 'r', '2', 0,
  /* 21 */ 'r', '1', '3', 0,
  /* 25 */ 'r', '3', 0,
  /* 28 */ 'r', '1', '4', 0,
  /* 32 */ 'r', '4', 0,
  /* 35 */ 'r', '1', '5', 0,
  /* 39 */ 'r', '5', 0,
  /* 42 */ 'r', '6', 0,
  /* 45 */ 'r', '7', 0,
  /* 48 */ 'r', '8', 0,
  /* 51 */ 'r', '9', 0,
  };

  static const uint8_t RegAsmOffset[] = {
    25, 25, 32, 32, 4, 4, 11, 11, 18, 18, 39, 42, 45, 48, 
    51, 0, 7, 14, 21, 28, 35, 39, 42, 45, 48, 51, 0, 7, 
    14, 21, 28, 35, 
  };

  assert (*(AsmStrs+RegAsmOffset[RegNo-1]) &&
          "Invalid alt name index for register!");
  return AsmStrs+RegAsmOffset[RegNo-1];
}

#ifdef PRINT_ALIAS_INSTR
#undef PRINT_ALIAS_INSTR

bool MSP430InstPrinter::printAliasInstr(const MCInst *MI, raw_ostream &OS) {
  const char *AsmString;
  switch (MI->getOpcode()) {
  default: return false;
  case MSP430::ADD16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (ADD16mc memdst:$dst, 1)
      AsmString = "inc	$\xFF\x01\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (ADD16mc memdst:$dst, 2)
      AsmString = "incd	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::ADD16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (ADD16rc GR16:$dst, 1)
      AsmString = "inc	$\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (ADD16rc GR16:$dst, 2)
      AsmString = "incd	$\x01";
      break;
    }
    return false;
  case MSP430::ADD8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (ADD8mc memdst:$dst, 1)
      AsmString = "inc.b	$\xFF\x01\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (ADD8mc memdst:$dst, 2)
      AsmString = "incd.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::ADD8rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (ADD8rc GR8:$dst, 1)
      AsmString = "inc.b	$\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (ADD8rc GR8:$dst, 2)
      AsmString = "incd.b	$\x01";
      break;
    }
    return false;
  case MSP430::ADDC16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (ADDC16mc memdst:$dst, 0)
      AsmString = "adc	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::ADDC16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (ADDC16rc GR16:$dst, 0)
      AsmString = "adc	$\x01";
      break;
    }
    return false;
  case MSP430::ADDC8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (ADDC8mc memdst:$dst, 0)
      AsmString = "adc.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::ADDC8rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (ADDC8rc GR8:$dst, 0)
      AsmString = "adc.b	$\x01";
      break;
    }
    return false;
  case MSP430::BIC16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 8) {
      // (BIC16rc SR, 8)
      AsmString = "dint";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (BIC16rc SR, 1)
      AsmString = "clrc";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 4) {
      // (BIC16rc SR, 4)
      AsmString = "clrn";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (BIC16rc SR, 2)
      AsmString = "clrz";
      break;
    }
    return false;
  case MSP430::BIS16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 8) {
      // (BIS16rc SR, 8)
      AsmString = "eint";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (BIS16rc SR, 1)
      AsmString = "setc";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 4) {
      // (BIS16rc SR, 4)
      AsmString = "setn";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).getReg() == MSP430::SR &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (BIS16rc SR, 2)
      AsmString = "setz";
      break;
    }
    return false;
  case MSP430::CMP16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (CMP16mc memdst:$dst, 0)
      AsmString = "tst	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::CMP16rc:
    if (MI->getNumOperands() == 2 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(1).isImm() &&
        MI->getOperand(1).getImm() == 0) {
      // (CMP16rc GR16:$dst, 0)
      AsmString = "tst	$\x01";
      break;
    }
    return false;
  case MSP430::CMP8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (CMP8mc memdst:$dst, 0)
      AsmString = "tst.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::CMP8rc:
    if (MI->getNumOperands() == 2 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(1).isImm() &&
        MI->getOperand(1).getImm() == 0) {
      // (CMP8rc GR8:$dst, 0)
      AsmString = "tst.b	$\x01";
      break;
    }
    return false;
  case MSP430::DADD16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (DADD16mc memdst:$dst, 0)
      AsmString = "dadc	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::DADD16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (DADD16rc GR16:$dst, 0)
      AsmString = "dadc	$\x01";
      break;
    }
    return false;
  case MSP430::DADD8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (DADD8mc memdst:$dst, 0)
      AsmString = "dadc.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::DADD8rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (DADD8rc GR8:$dst, 0)
      AsmString = "dadc.b	$\x01";
      break;
    }
    return false;
  case MSP430::MOV16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (MOV16mc memdst:$dst, 0)
      AsmString = "clr	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::MOV16rc:
    if (MI->getNumOperands() == 2 &&
        MI->getOperand(0).getReg() == MSP430::CG &&
        MI->getOperand(1).isImm() &&
        MI->getOperand(1).getImm() == 0) {
      // (MOV16rc CG, 0)
      AsmString = "nop";
      break;
    }
    if (MI->getNumOperands() == 2 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(1).isImm() &&
        MI->getOperand(1).getImm() == 0) {
      // (MOV16rc GR16:$dst, 0)
      AsmString = "clr	$\x01";
      break;
    }
    return false;
  case MSP430::MOV8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (MOV8mc memdst:$dst, 0)
      AsmString = "clr.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::MOV8rc:
    if (MI->getNumOperands() == 2 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(1).isImm() &&
        MI->getOperand(1).getImm() == 0) {
      // (MOV8rc GR8:$dst, 0)
      AsmString = "clr.b	$\x01";
      break;
    }
    return false;
  case MSP430::SUB16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (SUB16mc memdst:$dst, 1)
      AsmString = "dec	$\xFF\x01\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (SUB16mc memdst:$dst, 2)
      AsmString = "decd	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::SUB16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (SUB16rc GR16:$dst, 1)
      AsmString = "dec	$\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (SUB16rc GR16:$dst, 2)
      AsmString = "decd	$\x01";
      break;
    }
    return false;
  case MSP430::SUB8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (SUB8mc memdst:$dst, 1)
      AsmString = "dec.b	$\xFF\x01\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (SUB8mc memdst:$dst, 2)
      AsmString = "decd.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::SUB8rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 1) {
      // (SUB8rc GR8:$dst, 1)
      AsmString = "dec.b	$\x01";
      break;
    }
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 2) {
      // (SUB8rc GR8:$dst, 2)
      AsmString = "decd.b	$\x01";
      break;
    }
    return false;
  case MSP430::SUBC16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (SUBC16mc memdst:$dst, 0)
      AsmString = "sbc	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::SUBC16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (SUBC16rc GR16:$dst, 0)
      AsmString = "sbc	$\x01";
      break;
    }
    return false;
  case MSP430::SUBC8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (SUBC8mc memdst:$dst, 0)
      AsmString = "sbc.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::SUBC8rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      // (SUBC8rc GR8:$dst, 0)
      AsmString = "sbc.b	$\x01";
      break;
    }
    return false;
  case MSP430::XOR16mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == -1) {
      // (XOR16mc memdst:$dst, -1)
      AsmString = "inv	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::XOR16rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR16RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == -1) {
      // (XOR16rc GR16:$dst, -1)
      AsmString = "inv	$\x01";
      break;
    }
    return false;
  case MSP430::XOR8mc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == -1) {
      // (XOR8mc memdst:$dst, -1)
      AsmString = "inv.b	$\xFF\x01\x01";
      break;
    }
    return false;
  case MSP430::XOR8rc:
    if (MI->getNumOperands() == 3 &&
        MI->getOperand(0).isReg() &&
        MRI.getRegClass(MSP430::GR8RegClassID).contains(MI->getOperand(0).getReg()) &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == -1) {
      // (XOR8rc GR8:$dst, -1)
      AsmString = "inv.b	$\x01";
      break;
    }
    return false;
  }

  unsigned I = 0;
  while (AsmString[I] != ' ' && AsmString[I] != '\t' &&
         AsmString[I] != '$' && AsmString[I] != '\0')
    ++I;
  OS << '\t' << StringRef(AsmString, I);
  if (AsmString[I] != '\0') {
    if (AsmString[I] == ' ' || AsmString[I] == '\t') {
      OS << '\t';
      ++I;
    }
    do {
      if (AsmString[I] == '$') {
        ++I;
        if (AsmString[I] == (char)0xff) {
          ++I;
          int OpIdx = AsmString[I++] - 1;
          int PrintMethodIdx = AsmString[I++] - 1;
          printCustomAliasOperand(MI, OpIdx, PrintMethodIdx, OS);
        } else
          printOperand(MI, unsigned(AsmString[I++]) - 1, OS);
      } else {
        OS << AsmString[I++];
      }
    } while (AsmString[I] != '\0');
  }

  return true;
}

void MSP430InstPrinter::printCustomAliasOperand(
         const MCInst *MI, unsigned OpIdx,
         unsigned PrintMethodIdx,
         raw_ostream &OS) {
  switch (PrintMethodIdx) {
  default:
    llvm_unreachable("Unknown PrintMethod kind");
    break;
  case 0:
    printSrcMemOperand(MI, OpIdx, OS);
    break;
  }
}

#endif // PRINT_ALIAS_INSTR
