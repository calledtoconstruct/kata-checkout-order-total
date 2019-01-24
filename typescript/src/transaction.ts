
import * as uuid from 'uuid';
import { Item, ItemList } from './item';

class TransactionItem {

    constructor(private readonly item: Item) { }

    private howMany: number = 1;

    public code(): string {
        // TODO: Clean this up!  Given the Lint Rules, TypeScript will enforce this.
        if (this.item.code === null) {
            throw new Error('Invalid item: Missing required item code.');
        }
        return this.item.code;
    }

    public quantity(): number {
        return this.howMany;
    }

    public total(): number {
        return 0;
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

    public scan(code: string, weight?: number): number {
        const scanned: Item = this.itemList.get(code);

        if (scanned.type === 'by weight' && weight === undefined) {
            throw new Error('Weight is required for this type of item.');
        } else if (scanned.type === 'by quantity' && weight !== undefined) {
            throw new Error('Weight is not required for this type of item.');
        }

        const transactionItem: TransactionItem = new TransactionItem(scanned);
        this.item.push(transactionItem);
        return 0;
    }

    public remove(code: string): void {
        throw new Error('Item could not be removed because it does not exist.');
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