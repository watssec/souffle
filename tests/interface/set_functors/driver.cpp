/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file driver.cpp
 *
 * Driver program for invoking a Souffle program using the OO-interface
 *
 ***********************************************************************/

#include "souffle/SouffleInterface.h"
#include <array>
#include <memory>
#include <string>
#include <vector>

using namespace souffle;

/**
 * Error handler
 */
void error(std::string txt) {
    std::cerr << "error: " << txt << "\n";
    exit(1);
}

extern "C" {
    int32_t plus(int32_t x, int32_t y) {return x + y;}
    int32_t minus(int32_t x, int32_t y) {return x - y;}

    souffle::RamSigned cnt() {return 0;}
    souffle::RamSigned mul(souffle::RamSigned, souffle::RamSigned) {return 0;}
    souffle::RamDomain point_plus(souffle::SymbolTable*, souffle::RecordTable*, souffle::RamDomain, souffle::RamDomain) {return 0;}
}

// with an object
struct Obj {
    souffle::RamSigned mul(souffle::RamSigned x, souffle::RamSigned y) {
        cnt++;
        return x * y;
    }
    souffle::RamSigned count() {
        return cnt++;
    }
    std::atomic<souffle::RamSigned> cnt = 100;
};


/**
 * Main program
 */
int main(int argc, char** argv) {
    // check number of arguments
    if (argc != 2) {
        error("wrong number of arguments!");
    }
    Own<SouffleProgram> prog(ProgramFactory::newInstance("set_functors"));
    if (prog == nullptr) {
        error("failed to create souffle program");
    }

    std::function<souffle::RamSigned(souffle::RamSigned, souffle::RamSigned)> lambda_plus = plus;
    prog->setFunctor("plus", lambda_plus);

    Obj obj;

    std::function<souffle::RamSigned(souffle::RamSigned, souffle::RamSigned)> lambda_mul
        = std::bind(&Obj::mul, &obj, std::placeholders::_1, std::placeholders::_2);

    prog->setFunctor("mul", lambda_mul);
    obj.cnt = 0;

    std::function<souffle::RamSigned()> lambda_cnt
        = std::bind(&Obj::count, &obj);
    prog->setFunctor("cnt", lambda_cnt);

    souffle::RamDomain k = 10;

    std::function<souffle::RamDomain(souffle::SymbolTable*, souffle::RecordTable*, souffle::RamDomain, souffle::RamDomain)>
    lambda_point_plus = [&](souffle::SymbolTable*, souffle::RecordTable* recordTable, souffle::RamDomain x, souffle::RamDomain y) -> souffle::RamDomain {
        const souffle::RamDomain* pt_x = recordTable->unpack(x, 2);
        const souffle::RamDomain* pt_y = recordTable->unpack(y, 2);
        souffle::RamDomain res[2] = {pt_x[0] + pt_y[0] + k, pt_x[1] + pt_y[1] + k};
        return recordTable->pack(res, 2);
    };
    prog->setFunctor("point_plus", lambda_point_plus);

    k = 0;

    // run the program
    prog->run();

    prog->printAll();
}
