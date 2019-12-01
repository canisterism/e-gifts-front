import '../style/index.scss';
import { Elm } from '../src/Main.elm';

Elm.Main.init({
  node: document.querySelector('main'),
  flags: {
    innerWidth: window.innerWidth,
    innerHeight: window.innerHeight
  }
});
