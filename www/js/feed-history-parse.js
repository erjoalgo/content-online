var videoIdUpdateTimer = null;

var videoIdUpdateDelayMs = 250;

// text area for input
var textAreaId = "inner-html";
// read-only text area for display
var videoIdsTextAreaDisplayId = "video-ids-display";

var parsedIdsCountLabel = "parse-count";

function scheduleVideoIdUpdate (  ) {
    if (videoIdUpdateTimer) {
        clearTimeout(videoIdUpdateTimer);
        videoIdUpdateTimer = null;
    }
    videoIdUpdateTimer = setTimeout(videoIdUpdate, videoIdUpdateDelayMs);
}

function videoIdUpdate (  ) {
    var videoIds = parseVideoIds();
    var lines = videoIds.join("\n");
    document.getElementById(videoIdsTextAreaDisplayId).innerText = lines;
    // document.getElementById(videoIdsTextAreaId).innerText = lines;
    var lab = "parsed "+videoIds.length+" video ids";
    document.getElementById(parsedIdsCountLabel).innerText = lab;
}

function parseVideoIds ( text ) {
    if (!text) {
        text = document.getElementById(textAreaId).value;
    }

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

// var aggregationInputName
function postVideoIds (  ) {
    var aggregation = document.querySelector('input[name=aggregation]:checked');
    var videoIds = parseVideoIds();
    if (!aggregation) {
        alert("missing aggregation selection");
    } else if (videoIds.length === 0) {
        alert("empty video id list");
    } else  {
        var data = {
            aggregation: aggregation.value,
            videoIds: videoIds
        };
        console.log( "value of data: "+data );
        var xhr = new XMLHttpRequest();
        var postUrl = "/feed-history/video-ids";
        xhr.open('POST', postUrl, /* */true);
        xhr.setRequestHeader('Content-type', 'application/json');
        xhr.onreadystatechange = function() {
            if(xhr.readyState == 4){
                if ( xhr.status != 200) {
                    var err = "POST "+postUrl+" failed: "+xhr.status + " "+xhr.responseText;
                    console.log(err);
                    alert(err);
                    debugger;
                }else{
                    var resp = JSON.parse(xhr.responseText);
                    window.location = resp.location;
                }
            }
        };
        xhr.send(JSON.stringify(data));
    }
}
