
/************************************************************************
 *
 * @file Backend.h
 *
 * Backend that models the IR with SMT representations
 *
 ***********************************************************************/

#pragma once

#include "smt/Typing.h"

namespace souffle::smt {

/**
 * The backend of SMT modeling.
 */
class Backend {
public:
    // types
    virtual void mkTypeNumber(const TypeIndex& index) = 0;
    virtual void mkTypeUnsigned(const TypeIndex& index) = 0;
    virtual void mkTypeIdent(const TypeIndex& index, const std::string& name) = 0;
    virtual void mkTypeRecords(const ADTBuilderGroup& group) = 0;
};

}  // namespace souffle::smt