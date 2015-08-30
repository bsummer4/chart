// gapi_authorize ∷ (String, [String], Bool, Bool → JS a) → JS b
var gapi_authorize = function(cid,scope,immediate,cont) {
  var params = {client_id:cid, scope:scope, immediate:immediate};
  var authResult = gapi.auth.authorize(params,function (authResult) {
    cont(authResult && !authResult.error)
  });
};


var eventFields =
  [ 'items(start,end,colorId,description,summary)'
  , 'kind'
  , 'summary'
  ];


var allEventsStartingInAugust =
  { calendarId: 'primary'
  , timeMin: new Date(2015,7,1).toISOString()
  , timeMax: new Date(2015,8,1).toISOString()
  , showDeleted: false
  , singleEvents: true
  , maxResults: 2500
  , fields: eventFields.join(',')
  , orderBy: 'startTime'
  };


// august_events ∷ JSArray a → JS b
var august_events = function (cont) {
  gapi.client.calendar.events.list(allEventsStartingInAugust)
    .execute(function(response){
      if (!response || !('items' in response)) {
          return null }

      var events = response.items

      var getDate = function(e,k) {return new Date(e[k].dateTime || e[k].date)}

      var results = events.map(function(event) {
          return { start:       getDate(event, 'start')
                 , end:         getDate(event, 'end')
                 , color:       event.colorId     | 0
                 , summary:     event.summary     || ""
                 , description: event.description || ""
                 }});

      cont(results) })};
