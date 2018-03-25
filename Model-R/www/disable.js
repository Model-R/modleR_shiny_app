$(document).ready(function(){

	disableButtons(1, 5, "check", true); //Disable all
	disableButtons(1, 5, "check2", true); //Disable all


	$('.radio').click(function() {

	//Uncheck all
		if($('#radio1').prop('checked')) {
			disableButtons(1, 5, "check", true);
		} else {
			disableButtons(1, 5, "check", false);
		}

	//Uncheck custom
		if($('#tv1').prop('checked')){
			disableButtons(1, 5, "check2", true);
		}

		if($('#tv2').prop('checked')){
			disableButtons(1, 5, "check2", false); //Enable all
			disableButtons(3, 3, "check2", true); //Disable C
		}

		if($('#tv3').prop('checked')){
			disableButtons(1, 5, "check2", true); //Disable all
			disableButtons(3, 4, "check2", false); //Enable C, D
		}
	})
})

var disableButtons = function(from, to, id, dis) {
	for(i = from; i<= to; i++) {
		$('#' + id + "" + i).prop('disabled', dis);
	}
}
