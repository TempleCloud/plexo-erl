angular.module('plexoApp', []) .controller('MainCtrl', ['$http', function($http) {

	var self = this;

	self.apps = [];
	self.startNewApp = {};


	// Fetch the Apps
	var fetchApps = function() {
		// return self.apps;
		// return $http.get('http://localhost:8877/api/apps/running')
		return $http.get('http://localhost:8877/api/apps?status=running')
		.then(
			function(response) {
				self.apps = response.data;

//				console.log("Data JSON Stringified:" + JSON.stringify(self.apps));
//				var len = self.apps.length;
//				for (var i = 0; i < len; i++) {
//					var app = self.apps[i];
//					console.log("Stringify(app): " + JSON.stringify(app));
//					console.log("app.app_nfo.name: " + app.app_nfo.name);
//					console.log("app.app_nfo.description: " + app.app_nfo.description);
//					console.log("app.app_nfo.version: " + app.app_nfo.description);
//				}
//				console.log("Done");

			},
			function(errResponse) {
				console.error('Error while fetching api/apps');
			}
		);
	};

	fetchApps();

//	self.startApp = function() {
//		// POST - Create/Start a new App on the server...
//		$http.post('http://localhost:8877/api/app', self.startNewApp)
//		// THEN - Fetch the current Apps running on the server...
//		.then(fetchApps)
//		// THEN - reset the newTodo model.
//		.then(function(response) {
//			self.startNewApp = {};
//		});
//	};

}]);
