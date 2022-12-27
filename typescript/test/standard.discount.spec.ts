
import { Transaction } from '../src/transaction';
import { StandardItem, Item, Priced, ItemList, ItemListImplementation } from '../src/item';
import { StandardDiscount, Discount, DiscountList, DiscountListImplementation } from '../src/discount';

export class TestStandardDiscount {

    public static scenarios() {

        describe('Given a transaction', () => {

            let transaction: Transaction;

            describe('and a by quantity item', () => {

                const code = 'some by quantity item';
                const price = 2.97;
                const item: Item & Priced = new StandardItem(
                    code,
                    'random description',
                    'by quantity',
                    price
                );
                const discountPrice = 1.50;
                const today: number = new Date().valueOf();

                describe('and a standard discount for the same item', () => {

                    const discount: Discount = new StandardDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        discountPrice
                    );

                    beforeEach(async (): Promise<void> => {
                        const itemList: ItemList = new ItemListImplementation();
                        await itemList.add(item);
                        const discountList: DiscountList = new DiscountListImplementation(itemList);
                        await discountList.add(discount);
                        transaction = new Transaction(itemList, discountList);
                    });

                    describe('When calculating', () => {

                        let itemTotal: number;
                        let quantity: number;

                        beforeEach(async (): Promise<void> => {
                            itemTotal = await transaction.scan(code);
                            quantity = transaction.quantity(code);
                        });

                        it('Then the item total should be the quantity times the discount price.', () => {
                            expect(itemTotal).toEqual(quantity * discountPrice);
                        });

                    });

                });

            });

        });

    }

}
