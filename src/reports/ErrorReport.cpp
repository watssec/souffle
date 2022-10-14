/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file reports/ErrorReport.h
 *
 * Defines a class for error reporting.
 *
 ***********************************************************************/

#include "reports/ErrorReport.h"

namespace souffle {

std::optional<WarnType> warn_type_from_string(std::string s) {
    if (s == "deprecated-type-decl") {
        return std::optional<WarnType>(WarnType::DeprecatedTypeDecl);
    } else if (s == "deprecated-qualifier") {
        return std::optional<WarnType>(WarnType::DeprecatedQualifier);
    } else if (s == "dollar-sign") {
        return std::optional<WarnType>(WarnType::DollarSign);
    } else if (s == "no-rules-nor-facts") {
      return std::optional<WarnType>(WarnType::NoRulesNorFacts);
    } else if (s == "no-subsumptive-rule") {
      return std::optional<WarnType>(WarnType::NoSubsumptiveRule);
    } else if (s == "var-appears-once") {
      return std::optional<WarnType>(WarnType::VarAppearsOnce);
    }
    return std::optional<WarnType>();
}

};
