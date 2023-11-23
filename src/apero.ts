// @ts-ignore
import { Elm } from "./apero-core.js"


interface ElmFlags
{
    url: string;
}

export enum RequestTypes
{
    JsonLd = "json-ld",
    Turtle = "turtle",
    NQuads = "n-quads"
}

export class Apero
{
    private readonly _app;

    constructor(elementId: string, options: ElmFlags)
    {
        let _defaultOptions = {
            requestType: RequestTypes.JsonLd
        }

        // merge incoming options with a set of defaults.
        let _appOptions: ElmFlags = Object.assign(_defaultOptions, options);
        this._app = Elm.Apero.init({
            node: document.getElementById(elementId),
            flags: _appOptions
        });

        CommsCenter.setApp(this._app);

        // This captures messages from Elm and forwards them on to whoever is listening.
        // @ts-ignore
        CommsCenter.receiveFromElm(({message, params}) =>
        {
            const eventPayload = {
                "message": message,
                "params": params,
            };

            let apevent = new CustomEvent(`apero.${message}`, {detail: eventPayload});
            document.dispatchEvent(apevent);
        });
    }
}


class ElmCommunicator
{
    private _app;

    sendToElm(msg)
    {
        if (this._app === undefined)
        {
            return;
        }
        this._app.ports.receiveIncomingMessageFromPort.send(msg);
    }

    receiveFromElm(handlerFn)
    {
        if (this._app === undefined)
        {
            return;
        }
        this._app.ports.sendOutgoingMessageOnPort.subscribe(handlerFn);
    }

    setApp(app)
    {
        this._app = app;
    }
}


const CommsCenter = new ElmCommunicator();


export class AperoViewer extends HTMLElement
{
    constructor() {
        super();
    }
}

customElements.define("apero-viewer", AperoViewer as any);