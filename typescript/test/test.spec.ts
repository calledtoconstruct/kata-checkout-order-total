
import { TestItemList } from './item.spec';
import { TestDiscountList, TestDiscount } from './discount.spec';
import { TestTransaction } from './transaction.spec';
import { TestStandardDiscount } from './standard.discount.spec';
import { TestBulkFlatPriceDiscount } from './bulk.flat.price.discount.spec';
import { TestUpSalePercentDiscount } from './up.sale.percent.discount.spec';
import { TestLimitedUpSalePercentDiscount } from './limited.up.sale.percent.discount.spec';
import { TestUpSaleFlatPriceDiscount } from './up.sale.flat.price.discount.spec';

TestItemList.Scenarios();

TestDiscountList.Scenarios();

TestDiscount.Scenarios();

TestTransaction.Scenarios();

TestStandardDiscount.Scenarios();

TestBulkFlatPriceDiscount.Scenarios();

TestUpSalePercentDiscount.Scenarios();

TestLimitedUpSalePercentDiscount.Scenarios();

TestUpSaleFlatPriceDiscount.Scenarios();
