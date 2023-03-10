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
namespace RISCV {
enum {
  Feature64Bit = 0,
  FeatureRV32E = 1,
  FeatureRelax = 2,
  FeatureStdExtA = 3,
  FeatureStdExtC = 4,
  FeatureStdExtD = 5,
  FeatureStdExtF = 6,
  FeatureStdExtM = 7,
  NumSubtargetFeatures = 8
};
} // end namespace RISCV
} // end namespace llvm

#endif // GET_SUBTARGETINFO_ENUM


#ifdef GET_SUBTARGETINFO_MC_DESC
#undef GET_SUBTARGETINFO_MC_DESC

namespace llvm {
// Sorted (by key) array of values for CPU features.
extern const llvm::SubtargetFeatureKV RISCVFeatureKV[] = {
  { "64bit", "Implements RV64", RISCV::Feature64Bit, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "a", "'A' (Atomic Instructions)", RISCV::FeatureStdExtA, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "c", "'C' (Compressed Instructions)", RISCV::FeatureStdExtC, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "d", "'D' (Double-Precision Floating-Point)", RISCV::FeatureStdExtD, { { { 0x40ULL, 0x0ULL, 0x0ULL, } } } },
  { "e", "Implements RV32E (provides 16 rather than 32 GPRs)", RISCV::FeatureRV32E, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "f", "'F' (Single-Precision Floating-Point)", RISCV::FeatureStdExtF, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "m", "'M' (Integer Multiplication and Division)", RISCV::FeatureStdExtM, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
  { "relax", "Enable Linker relaxation.", RISCV::FeatureRelax, { { { 0x0ULL, 0x0ULL, 0x0ULL, } } } },
};

#ifdef DBGFIELD
#error "<target>GenSubtargetInfo.inc requires a DBGFIELD macro"
#endif
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
#define DBGFIELD(x) x,
#else
#define DBGFIELD(x)
#endif

// ===============================================================
// Data tables for the new per-operand machine model.

// {ProcResourceIdx, Cycles}
extern const llvm::MCWriteProcResEntry RISCVWriteProcResTable[] = {
  { 0,  0}, // Invalid
}; // RISCVWriteProcResTable

// {Cycles, WriteResourceID}
extern const llvm::MCWriteLatencyEntry RISCVWriteLatencyTable[] = {
  { 0,  0}, // Invalid
}; // RISCVWriteLatencyTable

// {UseIdx, WriteResourceID, Cycles}
extern const llvm::MCReadAdvanceEntry RISCVReadAdvanceTable[] = {
  {0,  0,  0}, // Invalid
}; // RISCVReadAdvanceTable

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

// Sorted (by key) array of values for CPU subtype.
extern const llvm::SubtargetSubTypeKV RISCVSubTypeKV[] = {
 { "generic-rv32", { { { 0x0ULL, 0x0ULL, 0x0ULL, } } }, &NoSchedModel },
 { "generic-rv64", { { { 0x1ULL, 0x0ULL, 0x0ULL, } } }, &NoSchedModel },
};

namespace RISCV_MC {
unsigned resolveVariantSchedClassImpl(unsigned SchedClass,
    const MCInst *MI, unsigned CPUID) {
  // Don't know how to resolve this scheduling class.
  return 0;
}
} // end of namespace RISCV_MC

struct RISCVGenMCSubtargetInfo : public MCSubtargetInfo {
  RISCVGenMCSubtargetInfo(const Triple &TT, 
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
    return RISCV_MC::resolveVariantSchedClassImpl(SchedClass, MI, CPUID); 
  }
};

static inline MCSubtargetInfo *createRISCVMCSubtargetInfoImpl(const Triple &TT, StringRef CPU, StringRef FS) {
  return new RISCVGenMCSubtargetInfo(TT, CPU, FS, RISCVFeatureKV, RISCVSubTypeKV, 
                      RISCVWriteProcResTable, RISCVWriteLatencyTable, RISCVReadAdvanceTable, 
                      nullptr, nullptr, nullptr);
}

} // end namespace llvm

#endif // GET_SUBTARGETINFO_MC_DESC


#ifdef GET_SUBTARGETINFO_TARGET_DESC
#undef GET_SUBTARGETINFO_TARGET_DESC

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

// ParseSubtargetFeatures - Parses features string setting specified
// subtarget options.
void llvm::RISCVSubtarget::ParseSubtargetFeatures(StringRef CPU, StringRef FS) {
  LLVM_DEBUG(dbgs() << "\nFeatures:" << FS);
  LLVM_DEBUG(dbgs() << "\nCPU:" << CPU << "\n\n");
  InitMCProcessorInfo(CPU, FS);
  const FeatureBitset& Bits = getFeatureBits();
  if (Bits[RISCV::Feature64Bit]) HasRV64 = true;
  if (Bits[RISCV::FeatureRV32E]) IsRV32E = true;
  if (Bits[RISCV::FeatureRelax]) EnableLinkerRelax = true;
  if (Bits[RISCV::FeatureStdExtA]) HasStdExtA = true;
  if (Bits[RISCV::FeatureStdExtC]) HasStdExtC = true;
  if (Bits[RISCV::FeatureStdExtD]) HasStdExtD = true;
  if (Bits[RISCV::FeatureStdExtF]) HasStdExtF = true;
  if (Bits[RISCV::FeatureStdExtM]) HasStdExtM = true;
}
#endif // GET_SUBTARGETINFO_TARGET_DESC


#ifdef GET_SUBTARGETINFO_HEADER
#undef GET_SUBTARGETINFO_HEADER

namespace llvm {
class DFAPacketizer;
namespace RISCV_MC {
unsigned resolveVariantSchedClassImpl(unsigned SchedClass, const MCInst *MI, unsigned CPUID);
}

struct RISCVGenSubtargetInfo : public TargetSubtargetInfo {
  explicit RISCVGenSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS);
public:
  unsigned resolveSchedClass(unsigned SchedClass,  const MachineInstr *DefMI, const TargetSchedModel *SchedModel) const override;
  unsigned resolveVariantSchedClass(unsigned SchedClass, const MCInst *MI, unsigned CPUID) const override;
  DFAPacketizer *createDFAPacketizer(const InstrItineraryData *IID) const;
  unsigned getHwMode() const override;
};
} // end namespace llvm

#endif // GET_SUBTARGETINFO_HEADER


#ifdef GET_SUBTARGETINFO_CTOR
#undef GET_SUBTARGETINFO_CTOR

#include "llvm/CodeGen/TargetSchedule.h"

namespace llvm {
extern const llvm::SubtargetFeatureKV RISCVFeatureKV[];
extern const llvm::SubtargetSubTypeKV RISCVSubTypeKV[];
extern const llvm::MCWriteProcResEntry RISCVWriteProcResTable[];
extern const llvm::MCWriteLatencyEntry RISCVWriteLatencyTable[];
extern const llvm::MCReadAdvanceEntry RISCVReadAdvanceTable[];
RISCVGenSubtargetInfo::RISCVGenSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS)
  : TargetSubtargetInfo(TT, CPU, FS, makeArrayRef(RISCVFeatureKV, 8), makeArrayRef(RISCVSubTypeKV, 2), 
                        RISCVWriteProcResTable, RISCVWriteLatencyTable, RISCVReadAdvanceTable, 
                        nullptr, nullptr, nullptr) {}

unsigned RISCVGenSubtargetInfo
::resolveSchedClass(unsigned SchedClass, const MachineInstr *MI, const TargetSchedModel *SchedModel) const {
  report_fatal_error("Expected a variant SchedClass");
} // RISCVGenSubtargetInfo::resolveSchedClass

unsigned RISCVGenSubtargetInfo
::resolveVariantSchedClass(unsigned SchedClass, const MCInst *MI, unsigned CPUID) const {
  return RISCV_MC::resolveVariantSchedClassImpl(SchedClass, MI, CPUID);
} // RISCVGenSubtargetInfo::resolveVariantSchedClass

unsigned RISCVGenSubtargetInfo::getHwMode() const {
  if (checkFeatures("-64bit")) return 1;
  if (checkFeatures("+64bit")) return 2;
  return 0;
}
} // end namespace llvm

#endif // GET_SUBTARGETINFO_CTOR


#ifdef GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS
#undef GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS

#endif // GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS


#ifdef GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS
#undef GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS

#endif // GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS

