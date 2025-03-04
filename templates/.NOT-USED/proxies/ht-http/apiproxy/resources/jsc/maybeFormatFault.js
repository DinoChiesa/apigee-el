// maybeFormatFault.js
// ------------------------------------------------------------------
//
// created: Wed Jul  7 10:29:20 2021
// last saved: <2021-July-07 10:30:26>

/* jshint esversion:6, node:false, strict:implied */
/* global context, response, print */

var handled = context.getVariable('fault_handled');
if ( ! handled ) {
  var error = response.content.asXML.error;
  var t = typeof error;
  print('typeof error: ' + t);
  if (t == 'undefined') {
    response.content = '<error><code>1001</code><message>unknown error</message></error>';
  }
  context.setVariable('fault_handled', true);
}
