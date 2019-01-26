
import * as uuid from 'uuid';
import { Item, Priced, ItemList } from './item';
import { DiscountList, Discount, DiscountItem } from './discount';

class TransactionItem {

    constructor(
        private readonly item: Item & Priced,
        private readonly itemWeight?: number
    ) { }

    private howMany: number = 1;

    public code(): string {
        return this.item.code;
    }

    public price(): number { 
        return this.item.price;
    }

    public weight(): number | undefined { 
        return this.itemWeight;
    }

    public quantity(): number {
        return this.howMany;
    }

    public decrement(): void {

    }

    public total(): number {
        const totalQuantity: number = this.itemWeight === undefined
            ? this.howMany
            : this.howMany * this.itemWeight;

        return totalQuantity * this.item.price;
    }
}

export class Transaction {

    constructor(
        private readonly itemList: ItemList,
        private readonly discountList: DiscountList
    ) {
    }

    private date: Date;
    private transaction: string;

    private readonly item: Array<TransactionItem> = new Array<TransactionItem>();

    public start(): string {
        this.date = new Date();

        const uniqueId: string = uuid();
        this.transaction = 'transaction ' + uniqueId;

        return this.transaction;
    }

    private static validateType(scanned: Item, weight?: number): void {
        if (scanned.type === 'by weight' && weight === undefined) {
            throw new Error('Weight is required for this type of item.');
        } else if (scanned.type === 'by quantity' && weight !== undefined) {
            throw new Error('Weight is not required for this type of item.');
        }
    }

    public scan(code: string, weight?: number): number {
        const scanned: Item & Priced = this.itemList.get(code);

        Transaction.validateType(scanned, weight);

        const transactionItem: TransactionItem = new TransactionItem(scanned, weight);
        this.item.push(transactionItem);

        const itemTotal: number = this.itemTotal(code);

        return itemTotal;
    }

    private itemTotal(code: string): number {
        const discount: Discount | undefined = this.discountList.get(this.date, code);
        const related: Array<TransactionItem> = this.items(code);
        
        if (discount === undefined) {        
            let total: number = 0;

            related.forEach((transactionItem: TransactionItem): void => {
                total += transactionItem.total();
            });

            return total;
        } else {
            const items: Array<DiscountItem> = new Array<DiscountItem>();
            related.forEach((item: TransactionItem): void => {
                const discountItem: DiscountItem = new DiscountItem(item.price(), item.quantity(), item.weight());
                items.push(discountItem);
            });
            return discount.total(items);
        }
    }

    private items(code: string): Array<TransactionItem> {
        return this.item.filter((value: TransactionItem): boolean => value.code() === code);
    }

    public void(code: string, weight?: number): void {
        const scanned: Item = this.itemList.get(code);

        Transaction.validateType(scanned, weight);

        const items: Array<TransactionItem> = this.items(code);

        if (items.length === 0) {
            throw new Error('Item could not be removed because it does not exist.');
        }

        const item: TransactionItem = items[0];

        if (item.quantity() > 1) {
            item.decrement();
        } else if (item.quantity() === 1) {
            const index: number = this.item.indexOf(item);
            this.item.splice(index, 1);
        } else {
            throw new Error('Invalid item quantity.');
        }
    }

    public total(): number {
        return 0;
    }

    public quantity(code: string): number {
        const matching: Array<TransactionItem> = this.item.filter((value: TransactionItem): boolean => value.code() === code);
        let count: number = 0;
        matching.forEach((value: TransactionItem): void => {
            count += value.quantity()
        });
        return count;
    }

}