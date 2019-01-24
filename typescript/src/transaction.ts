
import * as uuid from 'uuid';
import { Item } from './item';

export class Transaction {

    public start(): string {
        const uniqueId: string = uuid();
        return 'transaction ' + uniqueId;
    }

    public scan(item: Item): number {
        return 0;
    }

    public total(): number {
        return 0;
    }

    public includes(item: Item): boolean {
        return true;
    }

}