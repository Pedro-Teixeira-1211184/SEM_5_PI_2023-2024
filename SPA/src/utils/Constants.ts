export default class Constants {
    public static readonly API_BASE_URL = 'http://localhost:5050/';
    public static readonly API_AUTH_LOGIN_URL = Constants.API_BASE_URL + 'api/auth/signin';
    public static readonly API_AUTH_LOGOUT_URL = Constants.API_BASE_URL + 'api/auth/logout';
    public static readonly API_AUTH_AUTHENTICATED_URL = Constants.API_BASE_URL + 'api/auth/authenticated';

    public static CAMPUS_MANAGER_ROLE = 'Campus manager';
    public static ADMIN_ROLE = 'Admin';

}
