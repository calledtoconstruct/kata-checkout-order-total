
import * as express from 'express';
import * as env from './env';

// begin verify
import { ItemList } from "./item";
import { DiscountList } from "./discount";
import { Transaction } from "./transaction";
import { ItemListClient } from "./item.list.client";
import { DiscountListClient } from "./discount.list.client";

const itemList: ItemList = new ItemListClient();
const discountList: DiscountList = new DiscountListClient();
const transaction: Transaction = new Transaction(itemList, discountList);
// end verify

const application = express();
const port: number = env.APPLICATION_PORT;

const applicationScript: string = `<script src='/script'></script>`;
const head: string = `<head>${applicationScript}</head>`;
const labelForCode: string = `<label>Code: </label>`;
const inputForCode: string = `<input type='text' name='code' id='code' />`;
const form: string = `<form id='scanner'>${labelForCode}${inputForCode}</form>`;
const items: string = `<ul id='items'></ul>`;
const total: string = `<div><span>Total: </span><span id='total'></span></div>`;
const body: string = `<body>${items}${form}${total}</body>`;
const document: string = `<html>${head}${body}</html>`;

application.get('/', async (_: express.Request, response: express.Response): Promise<void> => {
    response.send(document);
});

application.get('/script', async (_: express.Request, response: express.Response): Promise<void> => {
    response.sendFile(__dirname + '/main.js');
});

application.get('/verify', async (_: express.Request, response: express.Response): Promise<void> => {
    transaction.start();
    let output = new Array<OperationResult>();
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
    const head: string = `<head></head>`;
    const totalAmount: number = await transaction.total();
    const total: string = `<div><span>Total: </span><span>${totalAmount}</span></div>`;
    const body: string = `<body>${items}${total}</body>`;
    const document: string = `<html>${head}${body}</html>`;
    response.send(document);
});

application.listen(port, (): void => {
    console.log('Application is Listening.');
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