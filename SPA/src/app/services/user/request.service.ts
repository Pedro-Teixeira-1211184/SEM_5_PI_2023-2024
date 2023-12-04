import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import {ISignUpRequestDTO} from "../../dto/ISignUpRequestDTO";

@Injectable({
  providedIn: 'root'
})
export class RequestService {

  constructor() { }

  public async getAllRequests(): Promise<any> {
    try {
      const response = await fetch(Constants.API_REQUEST_GET_ALL_URL, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json'
        }
      });

      if (response.status === 200) {
        return await response.json() as ISignUpRequestDTO[];
      } else {
        return [];
      }
    } catch (e) {
      console.log(e);
    }
  }
}
