console.log("loaded");
$(document).ready(function() {
  console.log("ready");
  console.log($("#submit_json").className);
  $( "#submit_json" ).click(function() {
    console.log("ping pong");
  });
});