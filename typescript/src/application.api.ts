
import * as express from 'express';
import * as env from './env';

// begin verify
import { ItemList } from './item';
import { DiscountList } from './discount';
import { Transaction } from './transaction';
import { ItemListClient } from './item.list.client';
import { DiscountListClient } from './discount.list.client';

const itemList: ItemList = new ItemListClient();
const discountList: DiscountList = new DiscountListClient();
const transaction: Transaction = new Transaction(itemList, discountList);
// end verify

const application = express();
const port: number = env.APPLICATION_PORT;

application.get('/', async (_: express.Request, response: express.Response): Promise<void> => {
    const applicationScript = `<script src='/script'></script>`;
    const head = `<head>${applicationScript}</head>`;
    const labelForCode = `<label>Code: </label>`;
    const inputForCode = `<input type='text' name='code' id='code' />`;
    const form = `<form id='scanner'>${labelForCode}${inputForCode}</form>`;
    const items = `<ul id='items'></ul>`;
    const total = `<div><span>Total: </span><span id='total'></span></div>`;
    const body = `<body>${items}${form}${total}</body>`;
    const document = `<html>${head}${body}</html>`;
    response.send(document);
});

application.get('/script', async (_: express.Request, response: express.Response): Promise<void> => {
    response.sendFile(__dirname + '/main.js');
});

class OperationResult {
    constructor(
        private operation: string,
        private result: boolean
    ) {}

    format(): string {
        const checked = this.result ? 'checked' : '';
        return `<p><input type='checkbox' ${checked} /> ${this.operation}</p>`;
    }
}

application.get('/verify', async (_: express.Request, response: express.Response): Promise<void> => {
    transaction.start();
    const output = new Array<OperationResult>();
    await transaction.scan('cat food');
    output.push(new OperationResult('Add one unit of cat food.', true));
    await transaction.scan('cat food');
    output.push(new OperationResult('Add one unit of cat food.', true));
    await transaction.scan('cat food');
    output.push(new OperationResult('Add one unit of cat food.', true));
    await transaction.scan('dog food');
    output.push(new OperationResult('Add one unit of dog food.', true));
    await transaction.scan('dog food');
    output.push(new OperationResult('Add one unit of dog food.', true));
    await transaction.scan('dog food');
    output.push(new OperationResult('Add one unit of dog food.', true));
    const items = output.map((operationResult: OperationResult) => operationResult.format()).join('');
    const head = `<head></head>`;
    const totalAmount: number = await transaction.total();
    const total = `<div><span>Total: </span><span>${totalAmount}</span></div>`;
    const body = `<body>${items}${total}</body>`;
    const document = `<html>${head}${body}</html>`;
    response.send(document);
});

application.listen(port, (): void => {
    console.log('Application is Listening.');
});
