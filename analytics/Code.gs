// Code.gs — MarineSensitivity usage log (bound to the log Sheet).
// Deploy: Deploy > New deployment > type "Web app",
//         execute as "Me", who has access "Anyone". Copy the /exec URL into
//         the MSENS_LOG_URL environment variable for the Shiny apps.
//
// The client (msens::ga_js) posts {rows:[{...}, ...]} as text/plain so the
// request stays CORS-simple (this endpoint does not answer OPTIONS).

var COLS = ["timestamp","app","app_version","client_id","session_id","event","params","page","referrer","user_agent"];

function doPost(e) {
  try {
    var sh   = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    var body = JSON.parse(e.postData.contents);
    var rows = body.rows || [body];
    if (!rows.length) return _ok(0);

    // one setValues() for the whole batch — far cheaper than appendRow() per event
    var values = rows.map(function (r) {
      return COLS.map(function (c) { return r[c] === undefined ? "" : r[c]; });
    });
    sh.getRange(sh.getLastRow() + 1, 1, values.length, COLS.length).setValues(values);
    return _ok(values.length);
  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({ ok: false, error: String(err) }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}

function _ok(n) {
  return ContentService
    .createTextOutput(JSON.stringify({ ok: true, n: n }))
    .setMimeType(ContentService.MimeType.JSON);
}
