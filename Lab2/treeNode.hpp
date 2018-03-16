#ifndef TREENODE_H
#define TREENODE_H

#include <string>
#include <vector>

#include "llvmHeaders.hpp"

class treeNode {
	public:
		std::string type;
		std::vector<treeNode*> children;

		virtual VALUE_TYPE codegen();

		treeNode() {
			type = "";
		}

		treeNode(std::string n) {
			type = n;
		}

		treeNode(treeNode &t) {
			type = t.type;
			children = t.children;
		}

		treeNode(treeNode *t) {
			type = t->type;
			children = t->children;
		}
};

class DeclNode : public treeNode {
	public:
		VALUE_TYPE codegen() override;

		DeclNode() {
			type = "";
		}

		DeclNode(std::string n) {
			type = n;
		}

		DeclNode(DeclNode &t) {
			type = t.type;
			children = t.children;
		}

		DeclNode(DeclNode *t) {
			type = t->type;
			children = t->children;
		}
};

class InitDeclNode : public treeNode {
	public:
		VALUE_TYPE codegen(LLVMTypeRef type);

		InitDeclNode() {
			type = "";
		}

		InitDeclNode(std::string n) {
			type = n;
		}

		InitDeclNode(InitDeclNode &t) {
			type = t.type;
			children = t.children;
		}

		InitDeclNode(InitDeclNode *t) {
			type = t->type;
			children = t->children;
		}
};

class VariableNode : public treeNode {
	public:
		VALUE_TYPE codegen(LLVMTypeRef type);

		VariableNode() {
			type = "";
		}

		VariableNode(std::string n) {
			type = n;
		}

		VariableNode(VariableNode &t) {
			type = t.type;
			children = t.children;
		}

		VariableNode(VariableNode *t) {
			type = t->type;
			children = t->children;
		}
};

class PointerNode : public treeNode {
	public:
		VALUE_TYPE codegen(LLVMTypeRef type);

		PointerNode() {
			type = "";
		}

		PointerNode(std::string n) {
			type = n;
		}

		PointerNode(PointerNode &t) {
			type = t.type;
			children = t.children;
		}

		PointerNode(PointerNode *t) {
			type = t->type;
			children = t->children;
		}
};

class ArrayNode : public treeNode {
	public:
		VALUE_TYPE codegen(LLVMTypeRef type);

		ArrayNode() {
			type = "";
		}

		ArrayNode(std::string n) {
			type = n;
		}

		ArrayNode(ArrayNode &t) {
			type = t.type;
			children = t.children;
		}

		ArrayNode(ArrayNode *t) {
			type = t->type;
			children = t->children;
		}
};

class FunctionNode : public treeNode {
	public:
		FUNCTION_TYPE codegen(LLVMTypeRef type);

		FunctionNode() {
			type = "";
		}

		FunctionNode(std::string n) {
			type = n;
		}

		FunctionNode(FunctionNode &t) {
			type = t.type;
			children = t.children;
		}

		FunctionNode(FunctionNode *t) {
			type = t->type;
			children = t->children;
		}
};

class ConstNode : public treeNode {
	public:
		std::string name;
		int ival;
		float fval;
		std::string sval;

		VALUE_TYPE codegen() override;

		ConstNode() {
			type = "Const";
		}

		ConstNode(int i) {
			type = "Const";
			name = "INT";
			ival = i;
		}

		ConstNode(float f) {
			type = "Const";
			name = "FLOAT";
			fval = f;
		}

		ConstNode(std::string s) {
			type = "Const";
			name = "STRING";
			sval = s;
		}
};

class IdentNode : public treeNode {
	public:
		std::string name;

		VALUE_TYPE codegen() override;

		IdentNode() {
			type = "Ident";
		}

		IdentNode(std::string n) {
			type = "Ident";
			name = n;
		}
};

#endif