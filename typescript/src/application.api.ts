
import * as express from 'express';
import * as env from './env';

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

application.listen(port, (): void => {
    console.log('Application is Listening.');
});
