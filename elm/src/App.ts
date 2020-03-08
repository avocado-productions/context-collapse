import { Elm } from './App.elm';

const app = Elm.App.init({
    node: document.querySelector('main'),
    flags: localStorage.getItem('markup'),
});
