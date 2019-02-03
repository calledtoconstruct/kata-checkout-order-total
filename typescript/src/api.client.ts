
class ActiveXObject {
    constructor(_: string) { }
}

const factories = [
    function () { return new XMLHttpRequest() },
    function () { return new ActiveXObject("Msxml3.XMLHTTP") },
    function () { return new ActiveXObject("Msxml2.XMLHTTP.6.0") },
    function () { return new ActiveXObject("Msxml2.XMLHTTP.3.0") },
    function () { return new ActiveXObject("Msxml2.XMLHTTP") },
    function () { return new ActiveXObject("Microsoft.XMLHTTP") }
];

const createClient = (): any => {
    let client: any = null;

    factories.forEach((factory: () => any): void => {
        if (null === client) {
            try {
                client = factory();
            } catch {
            }
        }
    });

    return client;
}

export const sendRequest = async <Response>(url: string, postData?: any): Promise<Response> => {
    return new Promise<Response>((resolve: (response: Response) => void, reject: (reason?: any) => void): void => {

        var client: any = createClient();

        if (client) {
            const method = (postData) ? "POST" : "GET";

            client.open(method, url, true);

            if (postData) {
                client.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
            }

            client.onreadystatechange = function () {
                if (client.readyState != 4) {
                    return;
                }

                if (client.status != 200 && client.status != 304) {
                    if (client.status === 404) {
                        reject(client.status);
                    }
                    return;
                }

                resolve(JSON.parse(client.response));
            }

            if (client.readyState == 4) {
                return;
            }

            client.send(postData);
        }

    });

}
