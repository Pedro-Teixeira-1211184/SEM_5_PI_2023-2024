export default class Constants {
    // user authentication
    public static readonly API_BASE_URL = 'http://localhost:5050/';
    public static readonly API_AUTH_LOGIN_URL = Constants.API_BASE_URL + 'api/auth/signin';
    public static readonly API_AUTH_LOGOUT_URL = Constants.API_BASE_URL + 'api/auth/logout';
    public static readonly API_AUTH_AUTHENTICATED_URL = Constants.API_BASE_URL + 'api/auth/authenticated';

    //building
    public static readonly API_BUILDING_CREATE_URL = Constants.API_BASE_URL + 'buildings';
    public static readonly API_BUILDING_EDIT_URL = Constants.API_BASE_URL + 'buildings';
    public static readonly API_BUILDING_GET_ALL_URL = Constants.API_BASE_URL + 'buildings';


    //floor
    public static readonly API_FLOOR_CREATE_URL = Constants.API_BASE_URL + 'buildings/floors';
    public static readonly API_FLOOR_GET_BY_BUILDING_CODE_URL = Constants.API_BASE_URL + 'buildings/floors/';


    //robot
    public static readonly API_ROBOT_GET_ALL_URL = Constants.API_BASE_URL + 'robots';
    public static readonly API_ROBOT_UPDATE_STATUS_URL = Constants.API_BASE_URL + 'robots/';

    //robot type
    public static readonly API_ROBOT_TYPE_CREATE_URL = Constants.API_BASE_URL + 'robots/types';


    //task
    public static readonly API_TASK_TYPE_CREATE_URL = Constants.API_BASE_URL + 'tasks/types';

    //passageway
    public static readonly API_PASSAGEWAY_CREATE_URL = Constants.API_BASE_URL + 'passageways';
    public static readonly API_PASSAGEWAY_GET_BY_FLOOR_CODE_URL = Constants.API_BASE_URL + 'passageways/';
    public static readonly API_PASSAGEWAY_GET_BETWEEN_BUILDINGS_URL = Constants.API_BASE_URL + 'passageways/passagewaysBetweenBuildings/';

    //map
    public static readonly API_MAP_PATCH_URL = Constants.API_BASE_URL + 'maps';

    //elevator
    public static readonly API_ELEVATOR_GET_BY_BUILDING_CODE_URL = 'elevators/';

    public static readonly CAMPUS_MANAGER_ROLE = 'Campus manager';
    public static readonly ADMIN_ROLE = 'Admin';
    public static readonly FLEET_MANAGER_ROLE = 'Fleet manager';


}
