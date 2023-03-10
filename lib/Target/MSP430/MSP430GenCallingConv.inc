/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Calling Convention Implementation Fragment                                 *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

static bool CC_MSP430_AssignStack(unsigned ValNo, MVT ValVT,
                                  MVT LocVT, CCValAssign::LocInfo LocInfo,
                                  ISD::ArgFlagsTy ArgFlags, CCState &State);
static bool RetCC_MSP430(unsigned ValNo, MVT ValVT,
                         MVT LocVT, CCValAssign::LocInfo LocInfo,
                         ISD::ArgFlagsTy ArgFlags, CCState &State);


static bool CC_MSP430_AssignStack(unsigned ValNo, MVT ValVT,
                                  MVT LocVT, CCValAssign::LocInfo LocInfo,
                                  ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (ArgFlags.isByVal()) {
    State.HandleByVal(ValNo, ValVT, LocVT, LocInfo, 2, 2, ArgFlags);
    return false;
  }

  if (LocVT == MVT::i8) {
    LocVT = MVT::i16;
    if (ArgFlags.isSExt())
        LocInfo = CCValAssign::SExt;
    else if (ArgFlags.isZExt())
        LocInfo = CCValAssign::ZExt;
    else
        LocInfo = CCValAssign::AExt;
  }

  if (LocVT == MVT::i16) {
    unsigned Offset1 = State.AllocateStack(2, 2);
    State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset1, LocVT, LocInfo));
    return false;
  }

  return true;  // CC didn't match.
}


static bool RetCC_MSP430(unsigned ValNo, MVT ValVT,
                         MVT LocVT, CCValAssign::LocInfo LocInfo,
                         ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i8) {
    static const MCPhysReg RegList1[] = {
      MSP430::R12B, MSP430::R13B, MSP430::R14B, MSP430::R15B
    };
    if (unsigned Reg = State.AllocateReg(RegList1)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::i16) {
    static const MCPhysReg RegList2[] = {
      MSP430::R12, MSP430::R13, MSP430::R14, MSP430::R15
    };
    if (unsigned Reg = State.AllocateReg(RegList2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  return true;  // CC didn't match.
}
