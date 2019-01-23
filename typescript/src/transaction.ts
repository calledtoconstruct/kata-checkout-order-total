
import * as uuid from 'uuid';

export class Transaction {

    public start(): string {
        const uniqueId: string = uuid();
        return 'transaction ' + uniqueId;
    }

}