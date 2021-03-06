import { Elm } from './src/Main.elm'

const audio = new Audio();
audio.loop = true;

const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: true
});

app.ports.playSong.subscribe(function (songName) {
  if (audio.src.endsWith(songName) && !audio.paused) {
    return;
  }
  audio.src = `music/${songName}`;
  audio.play();
});
app.ports.stop.subscribe(function () {
   audio.pause();
   audio.currentTime = 0;
});
