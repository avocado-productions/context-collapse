import { Elm } from './App.elm';

const script = new XMLHttpRequest();
script.open('GET', '/script.camp', true);
script.overrideMimeType("text/plain");
script.onreadystatechange = () => {
    if (script.readyState === XMLHttpRequest.DONE) {
        const app = Elm.App.init({
            node: document.querySelector('main'),
            flags: script.responseText,
        });
    }
};
script.send(null);