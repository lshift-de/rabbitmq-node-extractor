dispatcher_add(function(sammy) {
  sammy.get('#/node_extractor', function() {
    render({'extractionItems': {path:    '/node_extractor',
                                options: {sort: true}}},
           'node_extractor', '#/node_extractor');

  });
});

NAVIGATION['Node Extractor'] = ['#/node_extractor', "management"];


$('#download-settings').die().live('click', function() {
  var hostAndUserName = fetchCurrentSelection();

  var path = 'api/extract_settings' + hostAndUserName + '?download=export.json&auth=' + get_pref('auth');

  window.location = path;
  setTimeout('app.run()');

  return false;
});

$("input:checkbox[class=configCheckbox]").die().live('change', function() {
  var hostAndUserName = fetchCurrentSelection();
  var path = '/extract_settings' + hostAndUserName + '?auth=' + get_pref('auth');
  var configJson = sync_get(path);

  $('input[name=current-config]:hidden').val(configJson);
//  replace_content('debug', configJson);

  $('input[name=selected-hosts]:hidden').val(fetchVhosts());

  updateSectionVisibility(hostAndUserName);
  updateSubmitButton();
});

$("input[class=targetNodeCreds]").die().live('change', function() {

  var targetStatus = $("#targetStatus");
  var username = $("input[name='uname']").val();
  var password = $("input[name='pwd']").val();

  if (!username || !password) {
    targetStatus.html("username and password are needed!");
    return;
  }

  var hostname = $("input[name='hostname']").val();
  var port = $("input[name='mgmt-port']").val();

  if (!hostname || !port) {
    targetStatus.html("hostname and port is needed!");
    return;
  }

  var userpass = username + ":" + password;

  var userinfo = decodeURIComponent(userpass);
  var b64 = b64_encode_utf8(userinfo);
  var targetAuth = encodeURIComponent(b64);

  var targetHost = "http://" + hostname + ":" + port + "/";
  var targetUrl = targetHost + "api/definitions?auth=" + targetAuth;

  $("input[name='remote-url']").val(targetUrl);
  var data = new FormData($("form[name='status-form']")[0]);

  $.ajax({
    url: $("form[name='status-form']").attr('action') + '?auth=' + get_pref('auth'),
    type: 'POST',
    cache: false,
    contentType: false,
    processData: false,
    data : data,
    complete: function (xhr, status) {
      if (status === 'error') {
        targetStatus.html("Remote connection issue: \n" + xhr.responseText);
        $("input[name='target-url']").val("");
      }
      else {
        targetStatus.html("Remote host connection data is valid.");
        $("input[name='target-url']").val(targetUrl);
      }

      updateSubmitButton();
    }
  });
});

$('#upload-settings').die().live('click', function() {

  if ($('input[name=current-config]:hidden').val().length == 0) {
    return;
  }

  $("input[name='auth']").val(get_pref('auth'));

  var data = new FormData($("form[name='upload-form']")[0]);

  $("form[name='upload-form']").submit(function(event) {
    event.preventDefault();

    $.ajax({
      url: $("form[name='upload-form']").attr('action') + '?auth=' + get_pref('auth'),
      type: 'POST',
      cache: false,
      contentType: false,
      processData: false,
      data : data,
      complete: function (xhr, status) {
        if (status === 'error') {
          $("#result").text("Error in sending configuration! (" + xhr.responseText + ")");
        }
        else {
          $("#result").text("Extraction finished: " + xhr.responseText);
          console.log('form submitted. => ' + xhr.responseText);
        }
      }
    });
    return false;
  });
});

$('#sendSection h2').die('click');

function updateSubmitButton() {
  var uploadButton = $("#upload-settings")[0];
  var hostAndUserName = fetchCurrentSelection();
  var targetUrl = $("input[name='target-url']").val();

  if (hostAndUserName.startsWith("/") && hostAndUserName.length > 1 && targetUrl.length > 0) {
    uploadButton.disabled = false;
  } else {
    uploadButton.disabled = true;
  }
}

function updateSectionVisibility(vhostUsers) {
  if ((vhostUsers.startsWith("/") && vhostUsers.length > 1 && $('#sendSection').hasClass('section-invisible')) ||
      (vhostUsers.startsWith("/") && vhostUsers.length <= 1 && $('#sendSection').hasClass('section-visible'))) {
    toggle_visibility($('#sendSection div.hider'));
  }
}

function fetchCurrentSelection() {
  var usernames = new Array();
  $("input:checkbox[name=username]:checked").each(function() {
    usernames.push($(this).val());
  });

  var vhost = "/" + fetchVhosts();
  var username = usernames.length <= 0 ? "" : "/" + usernames.join("|");

  return vhost + username;
}

function fetchVhosts() {
  var vhosts = new Array();
  $("input:checkbox[name=vhost]:checked").each(function() {
    vhosts.push(esc($(this).val()));
  });

  return vhosts.join("|");
}
