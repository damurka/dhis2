$( document ).ready(function() {

  Shiny.addCustomMessageHandler('loginStatus', function(data) {
     let button = document.getElementById(data.id);
     if (button) {
        button.disabled = false;
        button.innerHTML = 'Sign In';
     }
  });

  Shiny.addCustomMessageHandler('resetSelectizeInput', (data) => {
    var select = $('#' + data.id).selectize();;
    if (select.length > 0) {
      var selectize = select[0].selectize;
      selectize.clear(); // Clears the current selection
      selectize.clearOptions(); // Clears all the options
    }
  });

});
