// @ts-ignore
import { Elm } from "./apero-core.js";
const _bc = new BroadcastChannel("apero");
export class Apero {
    constructor(elementId, flags) {
        let defaultOptions = {
            requestType: "json-ld"
        };
        let _appOptions = Object.assign(defaultOptions, flags);
        // @ts-ignore
        this._app = Elm.Apero.init({
            node: document.getElementById(elementId),
            flags: _appOptions
        });
        // This captures messages from Elm and forwards them on to whoever is listening.
        // @ts-ignore
        this._app.ports.sendOutgoingMessageOnPort.subscribe(({ message, params }) => {
            const eventPayload = {
                "message": message,
                "params": params,
            };
            _bc.postMessage(eventPayload);
        });
        _bc.onmessage = (evt) => {
            this._app.ports.receiveIncomingMessageFromPort.send(evt.data);
        };
    }
}
