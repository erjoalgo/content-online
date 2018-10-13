var videoIdUpdateTimer = null;

var videoIdUpdateDelayMs = 250;
var textAreaId = "inner-html";
var videoIdsTextAreaId = "video-ids";
var videoIdsTextAreaDisplayId = "video-ids-display";
var parsedIsCountLabel = "parse-count";

function scheduleVideoIdUpdate (  ) {
    if (videoIdUpdateTimer != null) {
        clearTimeout(videoIdUpdateTimer);
        videoIdUpdateTimer = null;
    }
    videoIdUpdateTimer = setTimeout(videoIdUpdate, videoIdUpdateDelayMs);
}

function videoIdUpdate (  ) {
    console.log( "updating..." );
    var text = document.getElementById(textAreaId).value;
    var videoIds = parseVideoIds(text);
    var lines = videoIds.join("\n");
    document.getElementById(videoIdsTextAreaDisplayId).innerText = lines;
    document.getElementById(videoIdsTextAreaId).innerText = lines;
    var lab = "parsed "+videoIds.length+" video ids";
    document.getElementById(parsedIsCountLabel).innerText = lab;
}

function parseVideoIds ( text ) {
    // /watch?v=q4...
    var matches = text.match(/watch[?]v=[^"\\&]+/g) || [];
    var unique = {};
    for (var i = 0; i<matches.length; i++) {
        var match = matches[i];
        var id = match.split("=")[1];
        unique[id] = true;
    }
    return Object.keys(unique);
}
