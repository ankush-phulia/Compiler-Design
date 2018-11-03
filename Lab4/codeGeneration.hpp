#ifndef CODEGENERATION_H
#define CODEGENERATION_H

#include <map>
#include <stack>
#include <string>

#include "llvmHeaders.hpp"
#include "treeNode.hpp"

extern LLVMModuleRef mod;
extern LLVMBuilderRef globalBuilder;
extern std::stack<std::map<std::string, FUNCTION_TYPE> > funcSymTable;
extern std::stack<std::map<std::string, VALUE_TYPE> > symTable;
extern std::stack<LLVMContextRef> contextStack;
extern std::stack<LLVMBuilderRef> builderStack;

extern LLVMTypeRef stringToLLVMType(std::string typeName, LLVMContextRef c);
extern void codegen(treeNode* AST);

#endif
