
import * as uuid from 'uuid';
import { Item, Priced, ItemList } from './item';

class TransactionItem {

    constructor(
        private readonly item: Item & Priced,
        private readonly weight?: number
    ) { }

    private howMany: number = 1;

    public code(): string {
        return this.item.code;
    }

    public quantity(): number {
        return this.howMany;
    }

    public decrement(): void {

    }

    public total(): number {
        const totalQuantity: number = this.weight === undefined
            ? this.howMany
            : this.howMany * this.weight;

        return totalQuantity * this.item.price;
    }
}

export class Transaction {

    constructor(
        private readonly itemList: ItemList
    ) {
    }

    private readonly item: Array<TransactionItem> = new Array<TransactionItem>();

    public start(): string {
        const uniqueId: string = uuid();
        return 'transaction ' + uniqueId;
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

        return transactionItem.total();
    }

    public void(code: string, weight?: number): void {
        const scanned: Item = this.itemList.get(code);

        Transaction.validateType(scanned, weight);

        const items: Array<TransactionItem> = this.item.filter((value: TransactionItem): boolean => value.code() === code);

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