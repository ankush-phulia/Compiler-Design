#ifndef LOCAL_OPT_H
#define LOCAL_OPT_H

#include <map>
#include <stack>
#include <string>

#include "llvmHeaders.hpp"
#include "treeNode.hpp"

void localOpt(LLVMModuleRef mod, LLVMBuilderRef globalBuilder);

#endif