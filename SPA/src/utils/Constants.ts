export default class Constants {
  // user authentication
  public static readonly API_BASE_URL = 'http://localhost:5050/';
  public static readonly API_AUTH_LOGIN_URL = Constants.API_BASE_URL + 'api/auth/signin';
  public static readonly API_AUTH_LOGOUT_URL = Constants.API_BASE_URL + 'api/auth/logout';
  public static readonly API_AUTH_AUTHENTICATED_URL = Constants.API_BASE_URL + 'api/auth/authenticated';

  //building
  public static readonly API_BUILDING_CREATE_URL = Constants.API_BASE_URL + 'buildings';

  public static CAMPUS_MANAGER_ROLE = 'Campus manager';
  public static ADMIN_ROLE = 'Admin';

}
