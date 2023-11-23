import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IMapDTO from "../../dto/IMapDTO";

@Injectable({
  providedIn: 'root'
})
export class MapService {

  constructor() {
  }

  public async getMaps(): Promise<IMapDTO[]> {
    try {
      const response = await fetch(Constants.API_GET_MAPS_URL);
      if (response.status === 200) {
        return await response.json();
      } else
        return [];
    } catch (error) {
      console.log(error);
      return [];
    }
  }
}
