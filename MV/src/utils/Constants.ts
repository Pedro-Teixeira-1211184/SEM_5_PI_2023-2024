export default class Constants {
    // user authentication
    public static readonly API_BASE_URL = 'http://localhost:5050/';
    public static readonly API_AUTH_LOGIN_URL = Constants.API_BASE_URL + 'api/auth/signin';
    public static readonly API_AUTH_LOGOUT_URL = Constants.API_BASE_URL + 'api/auth/logout';
    public static readonly API_AUTH_AUTHENTICATED_URL = Constants.API_BASE_URL + 'api/auth/authenticated';

    //map
    public static readonly API_GET_MAPS_URL = Constants.API_BASE_URL + 'maps/';

    //user
    public static readonly USER_ROLE = 'User';
    public static readonly API_AUTH_SIGNUP_URL = Constants.API_BASE_URL + 'api/auth/signup';


}
