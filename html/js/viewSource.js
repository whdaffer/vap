/*
The contents of this file are subject to the Netscape Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS
IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.

The Initial Developer of the Original Code is Bob Clary.

Contributor(s): Bob Clary, Original Work, Copyright 1999-2000
                Bob Clary, Netscape Communications, Copyright 2001

Alternatively, the contents of this file may be used under the
terms of the GNU Public License (the "GPL"), in which case the
provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only
under the terms of the GPL and not to allow others to use your
version of this file under the NPL, indicate your decision by
deleting the provisions above and replace them with the notice
and other provisions required by the GPL.  If you do not delete
the provisions above, a recipient may use your version of this
file under either the NPL or the GPL.
*/

function viewSource()
{
	var olderror = window.onerror;
	
	window.onerror = new Function("alert('your browser doesn\\'t support innerHTML, sorry!');");
	
	if (document.body && typeof(document.body.innerHTML) == 'undefined')
		alert('your browser doesn\'t support innerHTML, sorry!');
	else
	{
		var source = document.body.innerHTML;
		
		var html =  '<html><head><title>Cross-Browser Collapsible Lists Example Source</title></head>';
		html     += '<body><pre>' +  source.escapeHTML() + '</pre></body></html>';
		
		var win = window.open('about:blank', 'examples');
		win.document.write(html);
	}
	window.onerror = olderror;
}
