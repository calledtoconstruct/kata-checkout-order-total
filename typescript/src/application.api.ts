
import * as express from 'express';

const application = express();
const port: number = 8080;

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
    response.sendFile('/code/kata-checkout-order-total/typescript/application/main.js');
});

application.listen(port, (): void => {
    console.log('Application is Listening.');
});
