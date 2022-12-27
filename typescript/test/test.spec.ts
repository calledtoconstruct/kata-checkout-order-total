
import { TestItemList } from './item.spec';
import { TestDiscountList, TestDiscount } from './discount.spec';
import { TestTransaction } from './transaction.spec';
import { TestStandardDiscount } from './standard.discount.spec';
import { TestBulkFlatPriceDiscount } from './bulk.flat.price.discount.spec';
import { TestUpSalePercentDiscount } from './up.sale.percent.discount.spec';
import { TestLimitedUpSalePercentDiscount } from './limited.up.sale.percent.discount.spec';
import { TestUpSaleFlatPriceDiscount } from './up.sale.flat.price.discount.spec';
import { TestLimitedUpSaleFlatPriceDiscount } from './limited.up.sale.flat.price.discount.spec';
import { TestUpSalePercentDiscountByWeight } from './up.sale.percent.discount.by.weight.spec';
import { TestDiscountSerialization } from './discount.serialization.spec';

TestItemList.scenarios();

TestDiscountList.scenarios();

TestDiscount.scenarios();

TestTransaction.scenarios();

TestStandardDiscount.scenarios();

TestBulkFlatPriceDiscount.scenarios();

TestUpSalePercentDiscount.scenarios();

TestLimitedUpSalePercentDiscount.scenarios();

TestUpSaleFlatPriceDiscount.scenarios();

TestLimitedUpSaleFlatPriceDiscount.scenarios();

TestUpSalePercentDiscountByWeight.scenarios();

TestDiscountSerialization.scenarios();
