#include "backend/type.h"

#include "gtest/gtest.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/type.h"

namespace backend {
namespace {

TEST(ToLlvm, Primitive) {
  llvm::LLVMContext context;
  EXPECT_EQ(ToLlvmType(type::Bool, context), llvm::Type::getInt1Ty(context));
  EXPECT_EQ(ToLlvmType(type::I8, context), llvm::Type::getInt8Ty(context));
  EXPECT_EQ(ToLlvmType(type::U8, context), llvm::Type::getInt8Ty(context));
  EXPECT_EQ(ToLlvmType(type::I16, context), llvm::Type::getInt16Ty(context));
  EXPECT_EQ(ToLlvmType(type::U16, context), llvm::Type::getInt16Ty(context));
  EXPECT_EQ(ToLlvmType(type::I32, context), llvm::Type::getInt32Ty(context));
  EXPECT_EQ(ToLlvmType(type::U32, context), llvm::Type::getInt32Ty(context));
  EXPECT_EQ(ToLlvmType(type::I64, context), llvm::Type::getInt64Ty(context));
  EXPECT_EQ(ToLlvmType(type::U64, context), llvm::Type::getInt64Ty(context));
  EXPECT_EQ(ToLlvmType(type::F32, context),
            llvm::Type::getFloatTy(context));
  EXPECT_EQ(ToLlvmType(type::F64, context),
            llvm::Type::getDoubleTy(context));
  // TODO: Test for `type`
}

TEST(ToLlvm, Pointer) {
  llvm::LLVMContext context;
  EXPECT_EQ(ToLlvmType(type::Ptr(type::I32), context),
            llvm::Type::getInt32Ty(context)->getPointerTo());
  EXPECT_EQ(ToLlvmType(type::Ptr(type::Bool), context),
            llvm::Type::getInt1Ty(context)->getPointerTo());

  EXPECT_EQ(ToLlvmType(type::BufPtr(type::I32), context),
            llvm::Type::getInt32Ty(context)->getPointerTo());
  EXPECT_EQ(ToLlvmType(type::BufPtr(type::Bool), context),
            llvm::Type::getInt1Ty(context)->getPointerTo());

  EXPECT_EQ(ToLlvmType(type::Ptr(type::Ptr(type::I32)), context),
            llvm::Type::getInt32Ty(context)->getPointerTo()->getPointerTo());
  EXPECT_EQ(ToLlvmType(type::BufPtr(type::Ptr(type::I32)), context),
            llvm::Type::getInt32Ty(context)->getPointerTo()->getPointerTo());
  EXPECT_EQ(ToLlvmType(type::Ptr(type::BufPtr(type::I32)), context),
            llvm::Type::getInt32Ty(context)->getPointerTo()->getPointerTo());
  EXPECT_EQ(ToLlvmType(type::BufPtr(type::BufPtr(type::I32)), context),
            llvm::Type::getInt32Ty(context)->getPointerTo()->getPointerTo());
}

TEST(ToLlvm, Enum) {
  llvm::LLVMContext context;
  type::Enum e(nullptr);
  EXPECT_EQ(ToLlvmType(&e, context), llvm::Type::getInt64Ty(context));
}

TEST(ToLlvm, Flags) {
  llvm::LLVMContext context;
  type::Flags f(nullptr);
  EXPECT_EQ(ToLlvmType(&f, context), llvm::Type::getInt64Ty(context));
}

TEST(ToLlvm, Array) {
  llvm::LLVMContext context;

  EXPECT_EQ(ToLlvmType(type::Arr(3, type::I64), context),
            llvm::ArrayType::get(llvm::Type::getInt64Ty(context), 3));

  EXPECT_EQ(ToLlvmType(type::Arr(3, type::Arr(4, type::Bool)), context),
            llvm::ArrayType::get(
                llvm::ArrayType::get(llvm::Type::getInt1Ty(context), 4), 3));

  EXPECT_EQ(ToLlvmType(type::Arr(0, type::I64), context),
            llvm::ArrayType::get(llvm::Type::getInt64Ty(context), 0));
}

TEST(ToLlvm, FunctionNoReturn) {
  llvm::LLVMContext context;

  EXPECT_EQ(ToLlvmType(type::Func({}, {}), context),
            llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false));

  EXPECT_EQ(ToLlvmType(type::Func({core::AnonymousParam(
                                      type::QualType::NonConstant(type::Bool))},
                                  {}),
                       context),
            llvm::FunctionType::get(llvm::Type::getVoidTy(context),
                                    {llvm::Type::getInt1Ty(context)}, false));
  EXPECT_EQ(
      ToLlvmType(
          type::Func(
              {core::AnonymousParam(type::QualType::Constant(type::Bool))}, {}),
          context),
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false));

  EXPECT_EQ(
      ToLlvmType(
          type::Func(
              {core::AnonymousParam(type::QualType::NonConstant(type::I64)),
               core::AnonymousParam(type::QualType::NonConstant(type::Bool))},
              {}),
          context),
      llvm::FunctionType::get(
          llvm::Type::getVoidTy(context),
          {llvm::Type::getInt64Ty(context), llvm::Type::getInt1Ty(context)},
          false));
}

TEST(ToLlvm, FunctionOneRegisterSizedReturn) {
  llvm::LLVMContext context;

  EXPECT_EQ(
      ToLlvmType(type::Func({}, {type::I32}), context),
      llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {}, false));

  EXPECT_EQ(ToLlvmType(type::Func({core::AnonymousParam(
                                      type::QualType::NonConstant(type::Bool))},
                                  {type::I32}),
                       context),
            llvm::FunctionType::get(llvm::Type::getInt32Ty(context),
                                    {llvm::Type::getInt1Ty(context)}, false));

  EXPECT_EQ(
      ToLlvmType(type::Func({core::AnonymousParam(
                                type::QualType::Constant(type::Bool))},
                            {type::I32}),
                 context),
      llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {}, false));

  EXPECT_EQ(
      ToLlvmType(
          type::Func(
              {core::AnonymousParam(type::QualType::NonConstant(type::I64)),
               core::AnonymousParam(type::QualType::NonConstant(type::Bool))},
              {type::I32}),
          context),
      llvm::FunctionType::get(
          llvm::Type::getInt32Ty(context),
          {llvm::Type::getInt64Ty(context), llvm::Type::getInt1Ty(context)},
          false));
}

TEST(ToLlvm, FunctionOneLargeReturn) {
  llvm::LLVMContext context;

  EXPECT_EQ(ToLlvmType(type::Func({}, {type::Arr(100, type::I32)}), context),
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(context),
                {llvm::ArrayType::get(llvm::Type::getInt32Ty(context), 100)
                     ->getPointerTo()},
                false));

  EXPECT_EQ(ToLlvmType(type::Func({core::AnonymousParam(
                                      type::QualType::NonConstant(type::Bool))},
                                  {type::Arr(100, type::I32)}),
                       context),
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(context),
                {llvm::Type::getInt1Ty(context),
                 llvm::ArrayType::get(llvm::Type::getInt32Ty(context), 100)
                     ->getPointerTo()},
                false));

  EXPECT_EQ(ToLlvmType(type::Func({core::AnonymousParam(
                                      type::QualType::Constant(type::Bool))},
                                  {type::Arr(100, type::I32)}),
                       context),
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(context),
                {llvm::ArrayType::get(llvm::Type::getInt32Ty(context), 100)
                     ->getPointerTo()},
                false));

  EXPECT_EQ(
      ToLlvmType(
          type::Func(
              {core::AnonymousParam(type::QualType::NonConstant(type::I64)),
               core::AnonymousParam(type::QualType::NonConstant(type::Bool))},
              {type::Arr(100, type::I32)}),
          context),
      llvm::FunctionType::get(
          llvm::Type::getVoidTy(context),
          {llvm::Type::getInt64Ty(context), llvm::Type::getInt1Ty(context),
           llvm::ArrayType::get(llvm::Type::getInt32Ty(context), 100)
               ->getPointerTo()},
          false));
}

TEST(ToLlvm, FunctionMultipleReturns) {
  llvm::LLVMContext context;

  EXPECT_EQ(
      ToLlvmType(type::Func({}, {type::Bool, type::I8}), context),
      llvm::FunctionType::get(llvm::Type::getVoidTy(context),
                              {llvm::Type::getInt1Ty(context)->getPointerTo(),
                               llvm::Type::getInt8Ty(context)->getPointerTo()},
                              false));

  EXPECT_EQ(
      ToLlvmType(type::Func({core::AnonymousParam(
                                type::QualType::NonConstant(type::Bool))},
                            {type::Bool, type::I8}),
                 context),
      llvm::FunctionType::get(llvm::Type::getVoidTy(context),
                              {llvm::Type::getInt1Ty(context),
                               llvm::Type::getInt1Ty(context)->getPointerTo(),
                               llvm::Type::getInt8Ty(context)->getPointerTo()},
                              false));

  EXPECT_EQ(
      ToLlvmType(type::Func({core::AnonymousParam(
                                type::QualType::Constant(type::Bool))},
                            {type::Bool, type::I8}),
                 context),
      llvm::FunctionType::get(llvm::Type::getVoidTy(context),
                              {llvm::Type::getInt1Ty(context)->getPointerTo(),
                               llvm::Type::getInt8Ty(context)->getPointerTo()},
                              false));

  EXPECT_EQ(
      ToLlvmType(
          type::Func(
              {core::AnonymousParam(type::QualType::NonConstant(type::I64)),
               core::AnonymousParam(type::QualType::NonConstant(type::Bool))},
              {type::Bool, type::I8}),
          context),
      llvm::FunctionType::get(
          llvm::Type::getVoidTy(context),
          {llvm::Type::getInt64Ty(context), llvm::Type::getInt1Ty(context),
           llvm::Type::getInt1Ty(context)->getPointerTo(),
           llvm::Type::getInt8Ty(context)->getPointerTo()},
          false));
}

// TODO: structs

}  // namespace
}  // namespace backend
