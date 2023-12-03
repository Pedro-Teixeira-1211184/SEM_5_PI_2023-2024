export default class Constants {
  // user authentication
  public static readonly API_BASE_URL = 'http://localhost:5050/';
  public static readonly API_AUTH_LOGIN_URL = Constants.API_BASE_URL + 'api/auth/signin';
  public static readonly API_AUTH_SIGNUP_URL = Constants.API_BASE_URL + 'api/auth/signup';
  public static readonly API_AUTH_SIGNUP_REQUEST_URL = Constants.API_BASE_URL + 'api/auth/request';
  public static readonly API_AUTH_LOGOUT_URL = Constants.API_BASE_URL + 'api/auth/logout';
  public static readonly API_AUTH_DELETE_ACCOUNT_URL = Constants.API_BASE_URL + 'api/auth/delete/';
  public static readonly API_AUTH_GET_USER_BY_EMAIL = Constants.API_BASE_URL + 'api/auth/';

  //map
  public static readonly API_GET_MAPS_URL = Constants.API_BASE_URL + 'maps/';

}
