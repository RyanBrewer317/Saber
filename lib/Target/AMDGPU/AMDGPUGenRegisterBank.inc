/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Register Bank Source Fragments                                             *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifdef GET_REGBANK_DECLARATIONS
#undef GET_REGBANK_DECLARATIONS
namespace llvm {
namespace AMDGPU {
enum {
  SCCRegBankID,
  SGPRRegBankID,
  VCCRegBankID,
  VGPRRegBankID,
  NumRegisterBanks,
};
} // end namespace AMDGPU
} // end namespace llvm
#endif // GET_REGBANK_DECLARATIONS

#ifdef GET_TARGET_REGBANK_CLASS
#undef GET_TARGET_REGBANK_CLASS
private:
  static RegisterBank *RegBanks[];

protected:
  AMDGPUGenRegisterBankInfo();

#endif // GET_TARGET_REGBANK_CLASS

#ifdef GET_TARGET_REGBANK_IMPL
#undef GET_TARGET_REGBANK_IMPL
namespace llvm {
namespace AMDGPU {
const uint32_t SCCRegBankCoverageData[] = {
    // 0-31
    (1u << (AMDGPU::SReg_32RegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XEXEC_HIRegClassID - 0)) |
    (1u << (AMDGPU::SRegOrLds_32_and_SReg_1RegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XM0_XEXECRegClassID - 0)) |
    (1u << (AMDGPU::SGPR_32RegClassID - 0)) |
    (1u << (AMDGPU::TTMP_32RegClassID - 0)) |
    (1u << (AMDGPU::M0_CLASSRegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XM0RegClassID - 0)) |
    (1u << (AMDGPU::SCC_CLASSRegClassID - 0)) |
    0,
    // 32-63
    0,
    // 64-95
    0,
    // 96-127
    0,
};
const uint32_t SGPRRegBankCoverageData[] = {
    // 0-31
    (1u << (AMDGPU::SReg_32RegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XEXEC_HIRegClassID - 0)) |
    (1u << (AMDGPU::SRegOrLds_32_and_SReg_1RegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XM0_XEXECRegClassID - 0)) |
    (1u << (AMDGPU::SGPR_32RegClassID - 0)) |
    (1u << (AMDGPU::TTMP_32RegClassID - 0)) |
    (1u << (AMDGPU::M0_CLASSRegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XM0RegClassID - 0)) |
    (1u << (AMDGPU::SReg_64RegClassID - 0)) |
    (1u << (AMDGPU::SReg_1RegClassID - 0)) |
    (1u << (AMDGPU::VS_32RegClassID - 0)) |
    (1u << (AMDGPU::SRegOrLds_32RegClassID - 0)) |
    (1u << (AMDGPU::SReg_64_XEXECRegClassID - 0)) |
    (1u << (AMDGPU::SReg_1_XEXECRegClassID - 0)) |
    (1u << (AMDGPU::SGPR_64RegClassID - 0)) |
    (1u << (AMDGPU::CCR_SGPR_64RegClassID - 0)) |
    (1u << (AMDGPU::SReg_1_with_sub0RegClassID - 0)) |
    (1u << (AMDGPU::SReg_1_XEXEC_with_sub0RegClassID - 0)) |
    (1u << (AMDGPU::VS_64RegClassID - 0)) |
    (1u << (AMDGPU::SReg_1_with_sub0_with_sub0_in_SGPR_32RegClassID - 0)) |
    (1u << (AMDGPU::SReg_1_with_sub0_with_sub0_in_TTMP_32RegClassID - 0)) |
    0,
    // 32-63
    (1u << (AMDGPU::TTMP_64RegClassID - 32)) |
    (1u << (AMDGPU::SReg_128RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_128RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_128_with_sub0_sub1_sub2RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_96RegClassID - 32)) |
    (1u << (AMDGPU::SReg_96RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_96_with_sub0_sub1RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_128_with_sub0_sub1_sub2_and_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_96_with_sub0_sub1_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_128_with_sub1_sub2_sub3_and_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_96_with_sub1_sub2RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_96_with_sub1_sub2_with_sub1_sub2_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_128_with_sub1_sub2_sub3RegClassID - 32)) |
    (1u << (AMDGPU::TTMP_128RegClassID - 32)) |
    (1u << (AMDGPU::SReg_256RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_256RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160RegClassID - 32)) |
    (1u << (AMDGPU::SReg_160RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160_with_sub0_sub1_sub2RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160_with_sub0_sub1_sub2_and_SGPR_160_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160_with_sub1_sub2_sub3RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160_with_sub1_sub2_sub3_and_SGPR_160_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160_with_sub2_sub3_sub4RegClassID - 32)) |
    (1u << (AMDGPU::SGPR_160_with_sub2_sub3_sub4_and_SGPR_160_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 32)) |
    0,
    // 64-95
    (1u << (AMDGPU::SGPR_256_with_sub0_sub1_sub2RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub0_sub1_sub2_and_SGPR_256_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub4_sub5_sub6_sub7_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub1_sub2_sub3_and_SGPR_256_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub2_sub3_sub4_and_SGPR_256_with_sub4_sub5_sub6_sub7_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub2_sub3_sub4_and_SGPR_256_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub1_sub2_sub3RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_256_with_sub2_sub3_sub4RegClassID - 64)) |
    (1u << (AMDGPU::TTMP_256RegClassID - 64)) |
    (1u << (AMDGPU::SReg_512RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub0_sub1_sub2RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub0_sub1_sub2_and_SGPR_512_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub0_sub1_sub2_and_SGPR_512_with_sub8_sub9_sub10_sub11_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub4_sub5_sub6_sub7_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub8_sub9_sub10_sub11_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub12_sub13_sub14_sub15_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub2_sub3_sub4_and_SGPR_512_with_sub4_sub5_sub6_sub7_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub1_sub2_sub3_and_SGPR_512_with_sub12_sub13_sub14_sub15_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub1_sub2_sub3_and_SGPR_512_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub2_sub3_sub4_and_SGPR_512_with_sub0_sub1_sub2_sub3_in_SGPR_128_with_sub0_sub1_in_CCR_SGPR_64RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub2_sub3_sub4RegClassID - 64)) |
    (1u << (AMDGPU::SGPR_512_with_sub1_sub2_sub3RegClassID - 64)) |
    (1u << (AMDGPU::TTMP_512RegClassID - 64)) |
    0,
    // 96-127
    0,
};
const uint32_t VCCRegBankCoverageData[] = {
    // 0-31
    (1u << (AMDGPU::SReg_64RegClassID - 0)) |
    (1u << (AMDGPU::SReg_1RegClassID - 0)) |
    (1u << (AMDGPU::VS_32RegClassID - 0)) |
    (1u << (AMDGPU::SRegOrLds_32RegClassID - 0)) |
    (1u << (AMDGPU::SReg_32RegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XEXEC_HIRegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XM0RegClassID - 0)) |
    (1u << (AMDGPU::SRegOrLds_32_and_SReg_1RegClassID - 0)) |
    (1u << (AMDGPU::SReg_64_XEXECRegClassID - 0)) |
    (1u << (AMDGPU::SReg_1_XEXECRegClassID - 0)) |
    (1u << (AMDGPU::SReg_32_XM0_XEXECRegClassID - 0)) |
    (1u << (AMDGPU::SGPR_64RegClassID - 0)) |
    (1u << (AMDGPU::SGPR_32RegClassID - 0)) |
    (1u << (AMDGPU::CCR_SGPR_64RegClassID - 0)) |
    (1u << (AMDGPU::TTMP_32RegClassID - 0)) |
    0,
    // 32-63
    (1u << (AMDGPU::TTMP_64RegClassID - 32)) |
    0,
    // 64-95
    0,
    // 96-127
    0,
};
const uint32_t VGPRRegBankCoverageData[] = {
    // 0-31
    (1u << (AMDGPU::VGPR_32RegClassID - 0)) |
    (1u << (AMDGPU::VReg_1RegClassID - 0)) |
    (1u << (AMDGPU::VReg_64RegClassID - 0)) |
    (1u << (AMDGPU::AV_32RegClassID - 0)) |
    (1u << (AMDGPU::VS_32RegClassID - 0)) |
    (1u << (AMDGPU::VRegOrLds_32RegClassID - 0)) |
    (1u << (AMDGPU::AV_64RegClassID - 0)) |
    (1u << (AMDGPU::VS_64RegClassID - 0)) |
    0,
    // 32-63
    (1u << (AMDGPU::VReg_96RegClassID - 32)) |
    (1u << (AMDGPU::VReg_128RegClassID - 32)) |
    (1u << (AMDGPU::VReg_256RegClassID - 32)) |
    (1u << (AMDGPU::VReg_160RegClassID - 32)) |
    0,
    // 64-95
    (1u << (AMDGPU::VReg_512RegClassID - 64)) |
    0,
    // 96-127
    0,
};

RegisterBank SCCRegBank(/* ID */ AMDGPU::SCCRegBankID, /* Name */ "SCC", /* Size */ 32, /* CoveredRegClasses */ SCCRegBankCoverageData, /* NumRegClasses */ 114);
RegisterBank SGPRRegBank(/* ID */ AMDGPU::SGPRRegBankID, /* Name */ "SGPR", /* Size */ 512, /* CoveredRegClasses */ SGPRRegBankCoverageData, /* NumRegClasses */ 114);
RegisterBank VCCRegBank(/* ID */ AMDGPU::VCCRegBankID, /* Name */ "VCC", /* Size */ 64, /* CoveredRegClasses */ VCCRegBankCoverageData, /* NumRegClasses */ 114);
RegisterBank VGPRRegBank(/* ID */ AMDGPU::VGPRRegBankID, /* Name */ "VGPR", /* Size */ 512, /* CoveredRegClasses */ VGPRRegBankCoverageData, /* NumRegClasses */ 114);
} // end namespace AMDGPU

RegisterBank *AMDGPUGenRegisterBankInfo::RegBanks[] = {
    &AMDGPU::SCCRegBank,
    &AMDGPU::SGPRRegBank,
    &AMDGPU::VCCRegBank,
    &AMDGPU::VGPRRegBank,
};

AMDGPUGenRegisterBankInfo::AMDGPUGenRegisterBankInfo()
    : RegisterBankInfo(RegBanks, AMDGPU::NumRegisterBanks) {
  // Assert that RegBank indices match their ID's
#ifndef NDEBUG
  unsigned Index = 0;
  for (const auto &RB : RegBanks)
    assert(Index++ == RB->getID() && "Index != ID");
#endif // NDEBUG
}
} // end namespace llvm
#endif // GET_TARGET_REGBANK_IMPL
