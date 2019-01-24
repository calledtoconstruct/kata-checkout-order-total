
import { TestItemList } from './item.spec';
import { TestDiscountList, TestDiscount } from './discount.spec';
import { TestTransaction } from './transaction.spec';
import { TestStandardDiscount } from './standard.discount.spec';

TestItemList.Scenarios();

TestDiscountList.Scenarios();

TestDiscount.Scenarios();

TestTransaction.Scenarios();

TestStandardDiscount.Scenarios();
