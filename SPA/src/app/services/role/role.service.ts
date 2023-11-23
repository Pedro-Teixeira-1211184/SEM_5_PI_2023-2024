import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IRoleDTO from "../../dto/IRoleDTO";

@Injectable({
  providedIn: 'root'
})
export class RoleService {

  constructor() { }

  public async getAllRoles(): Promise<IRoleDTO[]> {
    try {
      const response = await fetch(Constants.API_ROLES_GET_ALL_URL, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json'
        }
      });
      return await response.json();
    } catch (e) {
      console.log(e);
      return [];
    }
  }
}
