/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Subtarget Enumeration Source Fragment                                      *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/


#ifdef GET_SUBTARGETINFO_ENUM
#undef GET_SUBTARGETINFO_ENUM

namespace llvm {
namespace R600 {
enum {
  FeatureCFALUBug = 0,
  FeatureCaymanISA = 1,
  FeatureEvergreen = 2,
  FeatureFMA = 3,
  FeatureFP32Denormals = 4,
  FeatureFP64 = 5,
  FeatureFetchLimit8 = 6,
  FeatureFetchLimit16 = 7,
  FeatureLocalMemorySize0 = 8,
  FeatureLocalMemorySize32768 = 9,
  FeatureLocalMemorySize65536 = 10,
  FeatureNorthernIslands = 11,
  FeaturePromoteAlloca = 12,
  FeatureR600 = 13,
  FeatureR600ALUInst = 14,
  FeatureR700 = 15,
  FeatureVertexCache = 16,
  FeatureWavefrontSize16 = 17,
  FeatureWavefrontSize32 = 18,
  FeatureWavefrontSize64 = 19,
  NumSubtargetFeatures = 20
};
} // end namespace R600
} // end namespace llvm

#endif // GET_SUBTARGETINFO_ENUM


#ifdef GET_SUBTARGETINFO_MC_DESC
#undef GET_SUBTARGETINFO_MC_DESC

namespace llvm {
// Sorted (by key) array of values for CPU features.
extern const llvm::SubtargetFeatureKV R600FeatureKV[] = {
  { "HasVertexCache", "Specify use of dedicated vertex cache", R600::FeatureVertexCache, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "R600ALUInst", "Older version of ALU instructions encoding", R600::FeatureR600ALUInst, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "caymanISA", "Use Cayman ISA", R600::FeatureCaymanISA, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "cfalubug", "GPU has CF_ALU bug", R600::FeatureCFALUBug, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "evergreen", "EVERGREEN GPU generation", R600::FeatureEvergreen, { { { 0x280ULL, 0x0ULL, 0x0ULL, } } } },
  { "fetch16", "Limit the maximum number of fetches in a clause to 16", R600::FeatureFetchLimit16, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "fetch8", "Limit the maximum number of fetches in a clause to 8", R600::FeatureFetchLimit8, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "fmaf", "Enable single precision FMA (not as fast as mul+add, but fused)", R600::FeatureFMA, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "fp32-denormals", "Enable single precision denormal handling", R600::FeatureFP32Denormals, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "fp64", "Enable double precision operations", R600::FeatureFP64, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "localmemorysize0", "The size of local memory in bytes", R600::FeatureLocalMemorySize0, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "localmemorysize32768", "The size of local memory in bytes", R600::FeatureLocalMemorySize32768, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "localmemorysize65536", "The size of local memory in bytes", R600::FeatureLocalMemorySize65536, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "northern-islands", "NORTHERN_ISLANDS GPU generation", R600::FeatureNorthernIslands, { { { 0x80280ULL, 0x0ULL, 0x0ULL, } } } },
  { "promote-alloca", "Enable promote alloca pass", R600::FeaturePromoteAlloca, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "r600", "R600 GPU generation", R600::FeatureR600, { { { 0x4140ULL, 0x0ULL, 0x0ULL, } } } },
  { "r700", "R700 GPU generation", R600::FeatureR700, { { { 0x180ULL, 0x0ULL, 0x0ULL, } } } },
  { "wavefrontsize16", "The number of threads per wavefront", R600::FeatureWavefrontSize16, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "wavefrontsize32", "The number of threads per wavefront", R600::FeatureWavefrontSize32, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "wavefrontsize64", "The number of threads per wavefront", R600::FeatureWavefrontSize64, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
};

#ifdef DBGFIELD
#error "<target>GenSubtargetInfo.inc requires a DBGFIELD macro"
#endif
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
#define DBGFIELD(x) x,
#else
#define DBGFIELD(x)
#endif

// Functional units for "R600_VLIW5_Itin"
namespace R600_VLIW5_ItinFU {
  const unsigned ALU_X = 1 << 0;
  const unsigned ALU_Y = 1 << 1;
  const unsigned ALU_Z = 1 << 2;
  const unsigned ALU_W = 1 << 3;
  const unsigned TRANS = 1 << 4;
  const unsigned ALU_NULL = 1 << 5;
} // end namespace R600_VLIW5_ItinFU

// Functional units for "R600_VLIW4_Itin"
namespace R600_VLIW4_ItinFU {
  const unsigned ALU_X = 1 << 0;
  const unsigned ALU_Y = 1 << 1;
  const unsigned ALU_Z = 1 << 2;
  const unsigned ALU_W = 1 << 3;
  const unsigned ALU_NULL = 1 << 4;
} // end namespace R600_VLIW4_ItinFU

extern const llvm::InstrStage R600Stages[] = {
  { 0, 0, 0, llvm::InstrStage::Required }, // No itinerary
  { 1, R600_VLIW5_ItinFU::ALU_NULL, -1, (llvm::InstrStage::ReservationKinds)0 }, // 1
  { 1, R600_VLIW5_ItinFU::ALU_X | R600_VLIW5_ItinFU::ALU_Y | R600_VLIW5_ItinFU::ALU_Z | R600_VLIW5_ItinFU::ALU_W, -1, (llvm::InstrStage::ReservationKinds)0 }, // 2
  { 1, R600_VLIW5_ItinFU::ALU_X | R600_VLIW5_ItinFU::ALU_Y | R600_VLIW5_ItinFU::ALU_Z | R600_VLIW5_ItinFU::ALU_W | R600_VLIW5_ItinFU::TRANS, -1, (llvm::InstrStage::ReservationKinds)0 }, // 3
  { 1, R600_VLIW5_ItinFU::TRANS, -1, (llvm::InstrStage::ReservationKinds)0 }, // 4
  { 1, R600_VLIW5_ItinFU::ALU_X, -1, (llvm::InstrStage::ReservationKinds)0 }, // 5
  { 1, R600_VLIW4_ItinFU::ALU_NULL, -1, (llvm::InstrStage::ReservationKinds)0 }, // 6
  { 1, R600_VLIW4_ItinFU::ALU_X | R600_VLIW4_ItinFU::ALU_Y | R600_VLIW4_ItinFU::ALU_Z | R600_VLIW4_ItinFU::ALU_W, -1, (llvm::InstrStage::ReservationKinds)0 }, // 7
  { 0, 0, 0, llvm::InstrStage::Required } // End stages
};
extern const unsigned R600OperandCycles[] = {
  0, // No itinerary
  0 // End operand cycles
};
extern const unsigned R600ForwardingPaths[] = {
 0, // No itinerary
 0 // End bypass tables
};

static const llvm::InstrItinerary R600_VLIW5_Itin[] = {
  { 0, 0, 0, 0, 0 }, // 0 NoInstrModel
  { 1, 1, 2, 0, 0 }, // 1 NullALU
  { 1, 2, 3, 0, 0 }, // 2 VecALU
  { 1, 3, 4, 0, 0 }, // 3 AnyALU
  { 1, 4, 5, 0, 0 }, // 4 TransALU
  { 1, 5, 6, 0, 0 }, // 5 XALU
  { 0, uint16_t(~0U), uint16_t(~0U), uint16_t(~0U), uint16_t(~0U) }// end marker
};

static const llvm::InstrItinerary R600_VLIW4_Itin[] = {
  { 0, 0, 0, 0, 0 }, // 0 NoInstrModel
  { 1, 6, 7, 0, 0 }, // 1 NullALU
  { 1, 7, 8, 0, 0 }, // 2 VecALU
  { 1, 7, 8, 0, 0 }, // 3 AnyALU
  { 1, 6, 7, 0, 0 }, // 4 TransALU
  { 0, 0, 0, 0, 0 }, // 5 XALU
  { 0, uint16_t(~0U), uint16_t(~0U), uint16_t(~0U), uint16_t(~0U) }// end marker
};

// ===============================================================
// Data tables for the new per-operand machine model.

// {ProcResourceIdx, Cycles}
extern const llvm::MCWriteProcResEntry R600WriteProcResTable[] = {
  { 0,  0}, // Invalid
}; // R600WriteProcResTable

// {Cycles, WriteResourceID}
extern const llvm::MCWriteLatencyEntry R600WriteLatencyTable[] = {
  { 0,  0}, // Invalid
}; // R600WriteLatencyTable

// {UseIdx, WriteResourceID, Cycles}
extern const llvm::MCReadAdvanceEntry R600ReadAdvanceTable[] = {
  {0,  0,  0}, // Invalid
}; // R600ReadAdvanceTable

#undef DBGFIELD

static const llvm::MCSchedModel NoSchedModel = {
  MCSchedModel::DefaultIssueWidth,
  MCSchedModel::DefaultMicroOpBufferSize,
  MCSchedModel::DefaultLoopMicroOpBufferSize,
  MCSchedModel::DefaultLoadLatency,
  MCSchedModel::DefaultHighLatency,
  MCSchedModel::DefaultMispredictPenalty,
  false, // PostRAScheduler
  false, // CompleteModel
  0, // Processor ID
  nullptr, nullptr, 0, 0, // No instruction-level machine model.
  nullptr, // No Itinerary
  nullptr // No extra processor descriptor
};

static const llvm::MCSchedModel R600_VLIW5_ItinModel = {
  MCSchedModel::DefaultIssueWidth,
  MCSchedModel::DefaultMicroOpBufferSize,
  MCSchedModel::DefaultLoopMicroOpBufferSize,
  MCSchedModel::DefaultLoadLatency,
  MCSchedModel::DefaultHighLatency,
  MCSchedModel::DefaultMispredictPenalty,
  false, // PostRAScheduler
  false, // CompleteModel
  1, // Processor ID
  nullptr, nullptr, 0, 0, // No instruction-level machine model.
  R600_VLIW5_Itin,
  nullptr // No extra processor descriptor
};

static const llvm::MCSchedModel R600_VLIW4_ItinModel = {
  MCSchedModel::DefaultIssueWidth,
  MCSchedModel::DefaultMicroOpBufferSize,
  MCSchedModel::DefaultLoopMicroOpBufferSize,
  MCSchedModel::DefaultLoadLatency,
  MCSchedModel::DefaultHighLatency,
  MCSchedModel::DefaultMispredictPenalty,
  false, // PostRAScheduler
  false, // CompleteModel
  2, // Processor ID
  nullptr, nullptr, 0, 0, // No instruction-level machine model.
  R600_VLIW4_Itin,
  nullptr // No extra processor descriptor
};

// Sorted (by key) array of values for CPU subtype.
extern const llvm::SubtargetSubTypeKV R600SubTypeKV[] = {
 { "barts", { { { 0x10801ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "caicos", { { { 0x801ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "cayman", { { { 0x80aULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW4_ItinModel },
 { "cedar", { { { 0x50005ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "cypress", { { { 0x9000cULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "juniper", { { { 0x90004ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "r600", { { { 0x92000ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "r630", { { { 0x52000ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "redwood", { { { 0x90005ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "rs880", { { { 0x22000ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "rv670", { { { 0x92000ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "rv710", { { { 0x58000ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "rv730", { { { 0x58000ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "rv770", { { { 0x98000ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "sumo", { { { 0x80005ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
 { "turks", { { { 0x10801ULL, 0x0ULL, 0x0ULL, } } }, &R600_VLIW5_ItinModel },
};

namespace R600_MC {
unsigned resolveVariantSchedClassImpl(unsigned SchedClass,
    const MCInst *MI, unsigned CPUID) {
  // Don't know how to resolve this scheduling class.
  return 0;
}
} // end of namespace R600_MC

struct R600GenMCSubtargetInfo : public MCSubtargetInfo {
  R600GenMCSubtargetInfo(const Triple &TT, 
    StringRef CPU, StringRef FS, ArrayRef<SubtargetFeatureKV> PF,
    ArrayRef<SubtargetSubTypeKV> PD,
    const MCWriteProcResEntry *WPR,
    const MCWriteLatencyEntry *WL,
    const MCReadAdvanceEntry *RA, const InstrStage *IS,
    const unsigned *OC, const unsigned *FP) :
      MCSubtargetInfo(TT, CPU, FS, PF, PD,
                      WPR, WL, RA, IS, OC, FP) { }

  unsigned resolveVariantSchedClass(unsigned SchedClass,
      const MCInst *MI, unsigned CPUID) const override {
    return R600_MC::resolveVariantSchedClassImpl(SchedClass, MI, CPUID); 
  }
};

static inline MCSubtargetInfo *createR600MCSubtargetInfoImpl(const Triple &TT, StringRef CPU, StringRef FS) {
  return new R600GenMCSubtargetInfo(TT, CPU, FS, R600FeatureKV, R600SubTypeKV, 
                      R600WriteProcResTable, R600WriteLatencyTable, R600ReadAdvanceTable, 
                      R600Stages, R600OperandCycles, R600ForwardingPaths);
}

} // end namespace llvm

#endif // GET_SUBTARGETINFO_MC_DESC


#ifdef GET_SUBTARGETINFO_TARGET_DESC
#undef GET_SUBTARGETINFO_TARGET_DESC

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

// ParseSubtargetFeatures - Parses features string setting specified
// subtarget options.
void llvm::R600Subtarget::ParseSubtargetFeatures(StringRef CPU, StringRef FS) {
  LLVM_DEBUG(dbgs() << "\nFeatures:" << FS);
  LLVM_DEBUG(dbgs() << "\nCPU:" << CPU << "\n\n");
  InitMCProcessorInfo(CPU, FS);
  const FeatureBitset& Bits = getFeatureBits();
  if (Bits[R600::FeatureCFALUBug]) CFALUBug = true;
  if (Bits[R600::FeatureCaymanISA]) CaymanISA = true;
  if (Bits[R600::FeatureEvergreen] && Gen < R600Subtarget::EVERGREEN) Gen = R600Subtarget::EVERGREEN;
  if (Bits[R600::FeatureFMA]) FMA = true;
  if (Bits[R600::FeatureFP32Denormals]) FP32Denormals = true;
  if (Bits[R600::FeatureFP64]) FP64 = true;
  if (Bits[R600::FeatureFetchLimit8] && TexVTXClauseSize < 8) TexVTXClauseSize = 8;
  if (Bits[R600::FeatureFetchLimit16] && TexVTXClauseSize < 16) TexVTXClauseSize = 16;
  if (Bits[R600::FeatureLocalMemorySize0] && LocalMemorySize < 0) LocalMemorySize = 0;
  if (Bits[R600::FeatureLocalMemorySize32768] && LocalMemorySize < 32768) LocalMemorySize = 32768;
  if (Bits[R600::FeatureLocalMemorySize65536] && LocalMemorySize < 65536) LocalMemorySize = 65536;
  if (Bits[R600::FeatureNorthernIslands] && Gen < R600Subtarget::NORTHERN_ISLANDS) Gen = R600Subtarget::NORTHERN_ISLANDS;
  if (Bits[R600::FeaturePromoteAlloca]) EnablePromoteAlloca = true;
  if (Bits[R600::FeatureR600] && Gen < R600Subtarget::R600) Gen = R600Subtarget::R600;
  if (Bits[R600::FeatureR600ALUInst]) R600ALUInst = false;
  if (Bits[R600::FeatureR700] && Gen < R600Subtarget::R700) Gen = R600Subtarget::R700;
  if (Bits[R600::FeatureVertexCache]) HasVertexCache = true;
  if (Bits[R600::FeatureWavefrontSize16] && WavefrontSize < 16) WavefrontSize = 16;
  if (Bits[R600::FeatureWavefrontSize32] && WavefrontSize < 32) WavefrontSize = 32;
  if (Bits[R600::FeatureWavefrontSize64] && WavefrontSize < 64) WavefrontSize = 64;
}
#endif // GET_SUBTARGETINFO_TARGET_DESC


#ifdef GET_SUBTARGETINFO_HEADER
#undef GET_SUBTARGETINFO_HEADER

namespace llvm {
class DFAPacketizer;
namespace R600_MC {
unsigned resolveVariantSchedClassImpl(unsigned SchedClass, const MCInst *MI, unsigned CPUID);
}

struct R600GenSubtargetInfo : public TargetSubtargetInfo {
  explicit R600GenSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS);
public:
  unsigned resolveSchedClass(unsigned SchedClass,  const MachineInstr *DefMI, const TargetSchedModel *SchedModel) const override;
  unsigned resolveVariantSchedClass(unsigned SchedClass, const MCInst *MI, unsigned CPUID) const override;
  DFAPacketizer *createDFAPacketizer(const InstrItineraryData *IID) const;
};
} // end namespace llvm

#endif // GET_SUBTARGETINFO_HEADER


#ifdef GET_SUBTARGETINFO_CTOR
#undef GET_SUBTARGETINFO_CTOR

#include "llvm/CodeGen/TargetSchedule.h"

namespace llvm {
extern const llvm::SubtargetFeatureKV R600FeatureKV[];
extern const llvm::SubtargetSubTypeKV R600SubTypeKV[];
extern const llvm::MCWriteProcResEntry R600WriteProcResTable[];
extern const llvm::MCWriteLatencyEntry R600WriteLatencyTable[];
extern const llvm::MCReadAdvanceEntry R600ReadAdvanceTable[];
extern const llvm::InstrStage R600Stages[];
extern const unsigned R600OperandCycles[];
extern const unsigned R600ForwardingPaths[];
R600GenSubtargetInfo::R600GenSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS)
  : TargetSubtargetInfo(TT, CPU, FS, makeArrayRef(R600FeatureKV, 20), makeArrayRef(R600SubTypeKV, 16), 
                        R600WriteProcResTable, R600WriteLatencyTable, R600ReadAdvanceTable, 
                        R600Stages, R600OperandCycles, R600ForwardingPaths) {}

unsigned R600GenSubtargetInfo
::resolveSchedClass(unsigned SchedClass, const MachineInstr *MI, const TargetSchedModel *SchedModel) const {
  report_fatal_error("Expected a variant SchedClass");
} // R600GenSubtargetInfo::resolveSchedClass

unsigned R600GenSubtargetInfo
::resolveVariantSchedClass(unsigned SchedClass, const MCInst *MI, unsigned CPUID) const {
  return R600_MC::resolveVariantSchedClassImpl(SchedClass, MI, CPUID);
} // R600GenSubtargetInfo::resolveVariantSchedClass

} // end namespace llvm

#endif // GET_SUBTARGETINFO_CTOR


#ifdef GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS
#undef GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS

#endif // GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS


#ifdef GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS
#undef GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS

#endif // GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS

