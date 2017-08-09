/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <libsolidity/analysis/CallgraphBuilder.h>

#include <libsolidity/ast/ASTVisitor.h>

using namespace std;
using namespace dev;
using namespace dev::solidity;

class CallgraphBuilderVisitor: public ASTConstVisitor
{
public:
	CallgraphBuilderVisitor(map<ASTNode const*, CallgraphNode>& _nodes):
		m_nodes(_nodes)
	{}
	virtual bool visit(SourceUnit const& _node) override { return true; }
	virtual bool visit(PragmaDirective const& _node) override { return true; }
	virtual bool visit(ImportDirective const& _node) override { return true; }
	virtual bool visit(ContractDefinition const& _node) override { return true; }
	virtual bool visit(InheritanceSpecifier const& _node) override { return true; }
	virtual bool visit(StructDefinition const& _node) override { return true; }
	virtual bool visit(UsingForDirective const& _node) override { return true; }
	virtual bool visit(EnumDefinition const& _node) override { return true; }
	virtual bool visit(EnumValue const& _node) override { return true; }
	virtual bool visit(ParameterList const& _node) override { return true; }
	virtual bool visit(FunctionDefinition const& _node) override { return true; }
	virtual bool visit(VariableDeclaration const& _node) override { return true; }
	virtual bool visit(ModifierDefinition const& _node) override { return true; }
	virtual bool visit(ModifierInvocation const& _node) override { return true; }
	virtual bool visit(EventDefinition const& _node) override { return true; }
	virtual bool visit(TypeName const& _node) override { return true; }
	virtual bool visit(ElementaryTypeName const& _node) override { return true; }
	virtual bool visit(UserDefinedTypeName const& _node) override { return true; }
	virtual bool visit(FunctionTypeName const& _node) override { return true; }
	virtual bool visit(Mapping const& _node) override { return true; }
	virtual bool visit(ArrayTypeName const& _node) override { return true; }
	virtual bool visit(Block const& _node) override { return true; }
	virtual bool visit(PlaceholderStatement const& _node) override { return true; }
	virtual bool visit(IfStatement const& _node) override { return true; }
	virtual bool visit(WhileStatement const& _node) override { return true; }
	virtual bool visit(ForStatement const& _node) override { return true; }
	virtual bool visit(Continue const& _node) override { return true; }
	virtual bool visit(InlineAssembly const& _node) override { return true; }
	virtual bool visit(Break const& _node) override { return true; }
	virtual bool visit(Return const& _node) override { return true; }
	virtual bool visit(Throw const& _node) override { return true; }
	virtual bool visit(VariableDeclarationStatement const& _node) override { return true; }
	virtual bool visit(ExpressionStatement const& _node) override { return true; }
	virtual bool visit(Conditional const& _node) override { return true; }
	virtual bool visit(Assignment const& _node) override { return true; }
	virtual bool visit(TupleExpression const& _node) override { return true; }
	virtual bool visit(UnaryOperation const& _node) override { return true; }
	virtual bool visit(BinaryOperation const& _node) override { return true; }
	virtual bool visit(FunctionCall const& _node) override { return true; }
	virtual bool visit(NewExpression const& _node) override { return true; }
	virtual bool visit(MemberAccess const& _node) override { return true; }
	virtual bool visit(Identifier const& _node) override { return true; }
	virtual bool visit(ElementaryTypeNameExpression const& _node) override { return true; }

	virtual void endVisit(SourceUnit const& _node) override {}
	virtual void endVisit(PragmaDirective const& _node) override {}
	virtual void endVisit(ImportDirective const& _node) override {}
	virtual void endVisit(ContractDefinition const& _node) override {}
	virtual void endVisit(InheritanceSpecifier const& _node) override {}
	virtual void endVisit(UsingForDirective const& _node) override {}
	virtual void endVisit(StructDefinition const& _node) override {}
	virtual void endVisit(EnumDefinition const& _node) override {}
	virtual void endVisit(EnumValue const& _node) override {}
	virtual void endVisit(ParameterList const& _node) override {}
	virtual void endVisit(FunctionDefinition const& _node) override {}
	virtual void endVisit(VariableDeclaration const& _node) override {}
	virtual void endVisit(ModifierDefinition const& _node) override {}
	virtual void endVisit(ModifierInvocation const& _node) override {}
	virtual void endVisit(EventDefinition const& _node) override {}
	virtual void endVisit(TypeName const& _node) override {}
	virtual void endVisit(ElementaryTypeName const& _node) override {}
	virtual void endVisit(UserDefinedTypeName const& _node) override {}
	virtual void endVisit(FunctionTypeName const& _node) override {}
	virtual void endVisit(Mapping const& _node) override {}
	virtual void endVisit(ArrayTypeName const& _node) override {}
	virtual void endVisit(Block const& _node) override {}
	virtual void endVisit(PlaceholderStatement const& _node) override {}
	virtual void endVisit(IfStatement const& _node) override {}
	virtual void endVisit(WhileStatement const& _node) override {}
	virtual void endVisit(ForStatement const& _node) override {}
	virtual void endVisit(Continue const& _node) override {}
	virtual void endVisit(InlineAssembly const& _node) override {}
	virtual void endVisit(Break const& _node) override {}
	virtual void endVisit(Return const& _node) override {}
	virtual void endVisit(Throw const& _node) override {}
	virtual void endVisit(VariableDeclarationStatement const& _node) override {}
	virtual void endVisit(ExpressionStatement const& _node) override {}
	virtual void endVisit(Conditional const& _node) override {}
	virtual void endVisit(Assignment const& _node) override {}
	virtual void endVisit(TupleExpression const& _node) override {}
	virtual void endVisit(UnaryOperation const& _node) override {}
	virtual void endVisit(BinaryOperation const& _node) override {}
	virtual void endVisit(FunctionCall const& _functionCall) override
	{
		CallgraphNode n = m_nodes[&_functionCall.expression()];
		for (auto const& arg: _functionCall.arguments())
			n += m_nodes[arg.get()];
		m_nodes[&_functionCall] = n;

		for (auto const& arg: _functionCall.arguments())
			if (arg->annotation().type->dataStoredIn(DataLocation::Storage))
				m_nodes[&_functionCall].readsFromStorage = true;
		if (auto const* ft = dynamic_pointer_cast<FunctionType const*>(_functionCall.expression().annotation().type.get()))
			if (ft->bound() && oeu)

		if (_functionCall.annotation().kind != FunctionCallKind::FunctionCall)
			return;

		FunctionType const& function = dynamic_pointer_cast<FunctionType const&>(*_functionCall.expression().annotation().type);
		switch (function.kind())
		{
		case FunctionType::Kind::Internal:
		{
			// Calling convention: Caller pushes return address and arguments
			// Callee removes them and pushes return values

			eth::AssemblyItem returnLabel = m_context.pushNewTag();
			for (unsigned i = 0; i < arguments.size(); ++i)
			{
				arguments[i]->accept(*this);
				utils().convertType(*arguments[i]->annotation().type, *function.parameterTypes()[i]);
			}
			_functionCall.expression().accept(*this);
			unsigned parameterSize = CompilerUtils::sizeOnStack(function.parameterTypes());
			if (function.bound())
			{
				// stack: arg2, ..., argn, label, arg1
				unsigned depth = parameterSize + 1;
				utils().moveIntoStack(depth, function.selfType()->sizeOnStack());
				parameterSize += function.selfType()->sizeOnStack();
			}

			if (m_context.runtimeContext())
				// We have a runtime context, so we need the creation part.
				utils().rightShiftNumberOnStack(32, false);
			else
				// Extract the runtime part.
				m_context << ((u256(1) << 32) - 1) << Instruction::AND;

			m_context.appendJump(eth::AssemblyItem::JumpType::IntoFunction);
			m_context << returnLabel;

			unsigned returnParametersSize = CompilerUtils::sizeOnStack(function.returnParameterTypes());
			// callee adds return parameters, but removes arguments and return label
			m_context.adjustStackOffset(returnParametersSize - parameterSize - 1);
			break;
		}
		case FunctionType::Kind::External:
		case FunctionType::Kind::CallCode:
		case FunctionType::Kind::DelegateCall:
		case FunctionType::Kind::BareCall:
		case FunctionType::Kind::BareCallCode:
		case FunctionType::Kind::BareDelegateCall:
			_functionCall.expression().accept(*this);
			appendExternalFunctionCall(function, arguments);
			break;
		case FunctionType::Kind::Creation:
		{
			_functionCall.expression().accept(*this);
			solAssert(!function.gasSet(), "Gas limit set for contract creation.");
			solAssert(function.returnParameterTypes().size() == 1, "");
			TypePointers argumentTypes;
			for (auto const& arg: arguments)
			{
				arg->accept(*this);
				argumentTypes.push_back(arg->annotation().type);
			}
			ContractDefinition const* contract =
				&dynamic_cast<ContractType const&>(*function.returnParameterTypes().front()).contractDefinition();
			m_context.callLowLevelFunction(
				"$copyContractCreationCodeToMemory_" + contract->type()->identifier(),
				0,
				1,
				[contract](CompilerContext& _context)
				{
					// copy the contract's code into memory
					eth::Assembly const& assembly = _context.compiledContract(*contract);
					CompilerUtils(_context).fetchFreeMemoryPointer();
					// pushes size
					auto subroutine = _context.addSubroutine(make_shared<eth::Assembly>(assembly));
					_context << Instruction::DUP1 << subroutine;
					_context << Instruction::DUP4 << Instruction::CODECOPY;
					_context << Instruction::ADD;
				}
			);
			utils().encodeToMemory(argumentTypes, function.parameterTypes());
			// now on stack: memory_end_ptr
			// need: size, offset, endowment
			utils().toSizeAfterFreeMemoryPointer();
			if (function.valueSet())
				m_context << dupInstruction(3);
			else
				m_context << u256(0);
			m_context << Instruction::CREATE;
			// Check if zero (out of stack or not enough balance).
			m_context << Instruction::DUP1 << Instruction::ISZERO;
			m_context.appendConditionalRevert();
			if (function.valueSet())
				m_context << swapInstruction(1) << Instruction::POP;
			break;
		}
		case FunctionType::Kind::SetGas:
		{
			// stack layout: contract_address function_id [gas] [value]
			_functionCall.expression().accept(*this);

			arguments.front()->accept(*this);
			utils().convertType(*arguments.front()->annotation().type, IntegerType(256), true);
			// Note that function is not the original function, but the ".gas" function.
			// Its values of gasSet and valueSet is equal to the original function's though.
			unsigned stackDepth = (function.gasSet() ? 1 : 0) + (function.valueSet() ? 1 : 0);
			if (stackDepth > 0)
				m_context << swapInstruction(stackDepth);
			if (function.gasSet())
				m_context << Instruction::POP;
			break;
		}
		case FunctionType::Kind::SetValue:
			// stack layout: contract_address function_id [gas] [value]
			_functionCall.expression().accept(*this);
			// Note that function is not the original function, but the ".value" function.
			// Its values of gasSet and valueSet is equal to the original function's though.
			if (function.valueSet())
				m_context << Instruction::POP;
			arguments.front()->accept(*this);
			break;
		case FunctionType::Kind::Send:
		case FunctionType::Kind::Transfer:
			_functionCall.expression().accept(*this);
			// Provide the gas stipend manually at first because we may send zero ether.
			// Will be zeroed if we send more than zero ether.
			m_context << u256(eth::GasCosts::callStipend);
			arguments.front()->accept(*this);
			utils().convertType(
				*arguments.front()->annotation().type,
				*function.parameterTypes().front(), true
			);
			// gas <- gas * !value
			m_context << Instruction::SWAP1 << Instruction::DUP2;
			m_context << Instruction::ISZERO << Instruction::MUL << Instruction::SWAP1;
			appendExternalFunctionCall(
				FunctionType(
					TypePointers{},
					TypePointers{},
					strings(),
					strings(),
					FunctionType::Kind::BareCall,
					false,
					nullptr,
					false,
					false,
					true,
					true
				),
				{}
			);
			if (function.kind() == FunctionType::Kind::Transfer)
			{
				// Check if zero (out of stack or not enough balance).
				m_context << Instruction::ISZERO;
				m_context.appendConditionalRevert();
			}
			break;
		case FunctionType::Kind::Selfdestruct:
			arguments.front()->accept(*this);
			utils().convertType(*arguments.front()->annotation().type, *function.parameterTypes().front(), true);
			m_context << Instruction::SELFDESTRUCT;
			break;
		case FunctionType::Kind::Revert:
			m_context.appendRevert();
			break;
		case FunctionType::Kind::SHA3:
		{
			TypePointers argumentTypes;
			for (auto const& arg: arguments)
			{
				arg->accept(*this);
				argumentTypes.push_back(arg->annotation().type);
			}
			utils().fetchFreeMemoryPointer();
			utils().encodeToMemory(argumentTypes, TypePointers(), function.padArguments(), true);
			utils().toSizeAfterFreeMemoryPointer();
			m_context << Instruction::KECCAK256;
			break;
		}
		case FunctionType::Kind::Log0:
		case FunctionType::Kind::Log1:
		case FunctionType::Kind::Log2:
		case FunctionType::Kind::Log3:
		case FunctionType::Kind::Log4:
		{
			unsigned logNumber = int(function.kind()) - int(FunctionType::Kind::Log0);
			for (unsigned arg = logNumber; arg > 0; --arg)
			{
				arguments[arg]->accept(*this);
				utils().convertType(*arguments[arg]->annotation().type, *function.parameterTypes()[arg], true);
			}
			arguments.front()->accept(*this);
			utils().fetchFreeMemoryPointer();
			utils().encodeToMemory(
				{arguments.front()->annotation().type},
				{function.parameterTypes().front()},
				false,
				true);
			utils().toSizeAfterFreeMemoryPointer();
			m_context << logInstruction(logNumber);
			break;
		}
		case FunctionType::Kind::Event:
		{
			_functionCall.expression().accept(*this);
			auto const& event = dynamic_cast<EventDefinition const&>(function.declaration());
			unsigned numIndexed = 0;
			// All indexed arguments go to the stack
			for (unsigned arg = arguments.size(); arg > 0; --arg)
				if (event.parameters()[arg - 1]->isIndexed())
				{
					++numIndexed;
					arguments[arg - 1]->accept(*this);
					if (auto const& arrayType = dynamic_pointer_cast<ArrayType const>(function.parameterTypes()[arg - 1]))
					{
						utils().fetchFreeMemoryPointer();
						utils().encodeToMemory(
							{arguments[arg - 1]->annotation().type},
							{arrayType},
							false,
							true
						);
						utils().toSizeAfterFreeMemoryPointer();
						m_context << Instruction::KECCAK256;
					}
					else
						utils().convertType(
							*arguments[arg - 1]->annotation().type,
							*function.parameterTypes()[arg - 1],
							true
						);
				}
			if (!event.isAnonymous())
			{
				m_context << u256(h256::Arith(dev::keccak256(function.externalSignature())));
				++numIndexed;
			}
			solAssert(numIndexed <= 4, "Too many indexed arguments.");
			// Copy all non-indexed arguments to memory (data)
			// Memory position is only a hack and should be removed once we have free memory pointer.
			TypePointers nonIndexedArgTypes;
			TypePointers nonIndexedParamTypes;
			for (unsigned arg = 0; arg < arguments.size(); ++arg)
				if (!event.parameters()[arg]->isIndexed())
				{
					arguments[arg]->accept(*this);
					nonIndexedArgTypes.push_back(arguments[arg]->annotation().type);
					nonIndexedParamTypes.push_back(function.parameterTypes()[arg]);
				}
			utils().fetchFreeMemoryPointer();
			utils().encodeToMemory(nonIndexedArgTypes, nonIndexedParamTypes);
			// need: topic1 ... topicn memsize memstart
			utils().toSizeAfterFreeMemoryPointer();
			m_context << logInstruction(numIndexed);
			break;
		}
		case FunctionType::Kind::BlockHash:
		{
			arguments[0]->accept(*this);
			utils().convertType(*arguments[0]->annotation().type, *function.parameterTypes()[0], true);
			m_context << Instruction::BLOCKHASH;
			break;
		}
		case FunctionType::Kind::AddMod:
		case FunctionType::Kind::MulMod:
		{
			for (unsigned i = 0; i < 3; i ++)
			{
				arguments[2 - i]->accept(*this);
				utils().convertType(*arguments[2 - i]->annotation().type, IntegerType(256));
			}
			if (function.kind() == FunctionType::Kind::AddMod)
				m_context << Instruction::ADDMOD;
			else
				m_context << Instruction::MULMOD;
			break;
		}
		case FunctionType::Kind::ECRecover:
		case FunctionType::Kind::SHA256:
		case FunctionType::Kind::RIPEMD160:
		{
			_functionCall.expression().accept(*this);
			static const map<FunctionType::Kind, u256> contractAddresses{{FunctionType::Kind::ECRecover, 1},
															   {FunctionType::Kind::SHA256, 2},
															   {FunctionType::Kind::RIPEMD160, 3}};
			m_context << contractAddresses.find(function.kind())->second;
			for (unsigned i = function.sizeOnStack(); i > 0; --i)
				m_context << swapInstruction(i);
			appendExternalFunctionCall(function, arguments);
			break;
		}
		case FunctionType::Kind::ByteArrayPush:
		case FunctionType::Kind::ArrayPush:
		{
			_functionCall.expression().accept(*this);
			solAssert(function.parameterTypes().size() == 1, "");
			solAssert(!!function.parameterTypes()[0], "");
			TypePointer paramType = function.parameterTypes()[0];
			shared_ptr<ArrayType> arrayType =
				function.kind() == FunctionType::Kind::ArrayPush ?
				make_shared<ArrayType>(DataLocation::Storage, paramType) :
				make_shared<ArrayType>(DataLocation::Storage);
			// get the current length
			ArrayUtils(m_context).retrieveLength(*arrayType);
			m_context << Instruction::DUP1;
			// stack: ArrayReference currentLength currentLength
			m_context << u256(1) << Instruction::ADD;
			// stack: ArrayReference currentLength newLength
			m_context << Instruction::DUP3 << Instruction::DUP2;
			ArrayUtils(m_context).resizeDynamicArray(*arrayType);
			m_context << Instruction::SWAP2 << Instruction::SWAP1;
			// stack: newLength ArrayReference oldLength
			ArrayUtils(m_context).accessIndex(*arrayType, false);

			// stack: newLength storageSlot slotOffset
			arguments[0]->accept(*this);
			// stack: newLength storageSlot slotOffset argValue
			TypePointer type = arguments[0]->annotation().type->closestTemporaryType(arrayType->baseType());
			solAssert(type, "");
			utils().convertType(*arguments[0]->annotation().type, *type);
			utils().moveToStackTop(1 + type->sizeOnStack());
			utils().moveToStackTop(1 + type->sizeOnStack());
			// stack: newLength argValue storageSlot slotOffset
			if (function.kind() == FunctionType::Kind::ArrayPush)
				StorageItem(m_context, *paramType).storeValue(*type, _functionCall.location(), true);
			else
				StorageByteArrayElement(m_context).storeValue(*type, _functionCall.location(), true);
			break;
		}
		case FunctionType::Kind::ObjectCreation:
		{
			// Will allocate at the end of memory (MSIZE) and not write at all unless the base
			// type is dynamically sized.
			ArrayType const& arrayType = dynamic_cast<ArrayType const&>(*_functionCall.annotation().type);
			_functionCall.expression().accept(*this);
			solAssert(arguments.size() == 1, "");

			// Fetch requested length.
			arguments[0]->accept(*this);
			utils().convertType(*arguments[0]->annotation().type, IntegerType(256));

			// Stack: requested_length
			// Allocate at max(MSIZE, freeMemoryPointer)
			utils().fetchFreeMemoryPointer();
			m_context << Instruction::DUP1 << Instruction::MSIZE;
			m_context << Instruction::LT;
			auto initialise = m_context.appendConditionalJump();
			// Free memory pointer does not point to empty memory, use MSIZE.
			m_context << Instruction::POP;
			m_context << Instruction::MSIZE;
			m_context << initialise;

			// Stack: requested_length memptr
			m_context << Instruction::SWAP1;
			// Stack: memptr requested_length
			// store length
			m_context << Instruction::DUP1 << Instruction::DUP3 << Instruction::MSTORE;
			// Stack: memptr requested_length
			// update free memory pointer
			m_context << Instruction::DUP1 << arrayType.baseType()->memoryHeadSize();
			m_context << Instruction::MUL << u256(32) << Instruction::ADD;
			m_context << Instruction::DUP3 << Instruction::ADD;
			utils().storeFreeMemoryPointer();
			// Stack: memptr requested_length

			// Check if length is zero
			m_context << Instruction::DUP1 << Instruction::ISZERO;
			auto skipInit = m_context.appendConditionalJump();

			// We only have to initialise if the base type is a not a value type.
			if (dynamic_cast<ReferenceType const*>(arrayType.baseType().get()))
			{
				m_context << Instruction::DUP2 << u256(32) << Instruction::ADD;
				utils().zeroInitialiseMemoryArray(arrayType);
			}
			m_context << skipInit;
			m_context << Instruction::POP;
			break;
		}
		case FunctionType::Kind::Assert:
		case FunctionType::Kind::Require:
		{
			arguments.front()->accept(*this);
			utils().convertType(*arguments.front()->annotation().type, *function.parameterTypes().front(), false);
			// jump if condition was met
			m_context << Instruction::ISZERO << Instruction::ISZERO;
			auto success = m_context.appendConditionalJump();
			if (function.kind() == FunctionType::Kind::Assert)
				// condition was not met, flag an error
				m_context.appendInvalid();
			else
				m_context.appendRevert();
			// the success branch
			m_context << success;
			break;
		}
		default:
			solAssert(false, "Invalid function type.");
		}
	}
	virtual void endVisit(NewExpression const& _node) override
	{
		m_nodes[&_node] = m_nodes[&_node.typeName()];
	}
	virtual void endVisit(MemberAccess const& _memberAccess) override
	{
		m_nodes[&_memberAccess] = m_nodes[&_memberAccess.expression()];

		if (auto const& varDecl = dynamic_cast<VariableDeclaration const*>(_memberAccess.annotation().referencedDeclaration))
			m_nodes[&_memberAccess] += referencedVariable(*varDecl);

		ASTString const& member = _memberAccess.memberName();
		switch (_memberAccess.expression().annotation().type->category())
		{
		case Type::Category::Contract:
		case Type::Category::Integer:
			if (member == "balance" && !_memberAccess.annotation().referencedDeclaration)
				m_nodes[&_memberAccess].readsEnvironment = true;
			break;
		case Type::Category::Magic:
			// we can ignore the kind of magic and only look at the name of the member
			if (member != "data" && member != "sig")
				m_nodes[&_memberAccess].readsEnvironment = true;
			break;
		case Type::Category::Struct:
		{
			if (
				_memberAccess.expression().annotation().type->dataStoredIn(DataLocation::Storage) &&
				_memberAccess.annotation().type->isValueType()
			)
				m_nodes[&_memberAccess].readsStorage = true;
			break;
		}
		case Type::Category::Array:
		{
			auto const& type = dynamic_cast<ArrayType const&>(*_memberAccess.expression().annotation().type);
			if (member == "length" && type.isDynamicallySized() && type.dataStoredIn(DataLocation::Storage))
				m_nodes[&_memberAccess].readsStorage = true;
			break;
		}
		}
	}
	virtual void endVisit(IndexAccess const& _indexAccess) override
	{
		solAssert(_indexAccess.indexExpression(), "");
		m_nodes[&_indexAccess] = m_nodes[&_indexAccess.baseExpression()] + m_nodes[_indexAccess.indexExpression()];

		Type const& baseType = *_indexAccess.baseExpression().annotation().type;

		if (baseType.dataStoredIn(DataLocation::Storage) && _indexAccess.annotation().type->isValueType())
			m_nodes[&_indexAccess].readsStorage = true;
	}
	virtual void endVisit(Identifier const& _identifier) override
	{
		Declaration const* declaration = _identifier.annotation().referencedDeclaration;
		solAssert(declaration, "");

		if (VariableDeclaration const* varDecl = dynamic_cast<VariableDeclaration const*>(declaration))
		{
			m_nodes[&_identifier] = referencedVariable(*varDecl);
		}
		if (MagicVariableDeclaration const* magicVar = dynamic_cast<MagicVariableDeclaration const*>(declaration))
		{
			switch (magicVar->type()->category())
			{
			case Type::Category::Contract:
				// "this" or "super"
				if (!dynamic_cast<ContractType const&>(*magicVar->type()).isSuper())
					// reads the address
					m_nodes[&_identifier].readsEnvironment = true;
				break;
			case Type::Category::Integer:
				// "now"
				m_nodes[&_identifier].readsEnvironment = true;
				break;
			default:
				break;
			}
		}
	}
	virtual void endVisit(ElementaryTypeNameExpression const& _node) override { m_nodes[&_node]; }
	virtual void endVisit(Literal const& _node) override { m_nodes[&_node]; }

private:

	/// Used by identifiers or member access nodes that access a variable.
	CallgraphNode referencedVariable(VariableDeclaration const& _varDecl)
	{
		CallgraphNode r;
		r.reads.insert(_varDecl);
		if (_varDecl->isStateVariable() && !_varDecl->isConstant() && _varDecl.type()->isValueType())
			r.readsStorage = true;
		return r;
	}

	std::map<ASTNode const*, CallgraphNode>& m_nodes;
};

bool CallgraphBuilder::build(vector<ASTNode const&> const& _nodes)
{
	CallgraphBuilderVisitor v(m_nodes);

	for (auto const& node: _nodes)
		node.accept(v);
}
