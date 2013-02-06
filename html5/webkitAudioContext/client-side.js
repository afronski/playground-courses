var setup = function() {
    var soundRequest = new XMLHttpRequest();

    soundRequest.open("GET", "/sample.ogg", true);
    soundRequest.responseType = "arraybuffer";
    soundRequest.onload = function () {
        try {
            var context = new webkitAudioContext(),
                mainNode = context.createGainNode(0),
                clip = context.createBufferSource();

            mainNode.connect(context.destination);
            context.decodeAudioData(soundRequest.response, function (buffer) {
                clip.buffer = buffer;
                clip.gain.value = 1.0;
                clip.connect(mainNode);
                clip.loop = true;
                clip.noteOn(0);
            }, function () {});
        } catch(e) {
            console.warn("Web Audio API is not supported in this browser");
        }
    };

    soundRequest.send();
};

setup();