import { Transaction } from "../src/transaction";
import { StandardItem, Item, Priced, ItemList, ItemListImplementation } from "../src/item";
import { StandardDiscount, Discount, DiscountList, DiscountListImplementation } from "../src/discount";

export class TestStandardDiscount {

    public static Scenarios() {

        describe('Given a transaction', () => {

            let transaction: Transaction;

            describe('and a by quantity item', () => {

                const code: string = 'some by quantity item';
                const price: number = 2.97;
                const item: Item & Priced = new StandardItem(
                    code,
                    'random description',
                    'by quantity',
                    price
                );
                const discountPrice: number = 1.50;
                const today: number = new Date().valueOf();

                describe('and a standard discount for the same item', () => {

                    const discount: Discount = new StandardDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        discountPrice
                    );

                    beforeEach(() => {
                        const itemList: ItemList = new ItemListImplementation();
                        itemList.add(item);
                        const discountList: DiscountList = new DiscountListImplementation(itemList);
                        discountList.add(discount);
                        transaction = new Transaction(itemList, discountList);
                    });

                    describe('When calculating', () => {

                        let itemTotal: number;
                        let quantity: number;

                        beforeEach(() => {
                            itemTotal = transaction.scan(code);
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