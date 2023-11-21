import {Component, inject, OnInit} from '@angular/core';
import * as Constants from "../../utils/Constants";
import {AuthService} from "../services/auth.service";

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {
  public sidebarOpen = false;
  public title = 'SPA';
  protected readonly Constants = Constants;
  auth: AuthService = inject(AuthService);
  isCampusManager: boolean = false;
  isAdministrator: boolean = false;
  isFleetManager: boolean = false;
  
  home_body: boolean = true;
  createBuilding: boolean = false;
  editBuilding: boolean = false;
  listBuilding: boolean = false;
  createFloor: boolean = false;
  floorsByBuildingCode: boolean = false;
  editFloor: boolean = false;
  createElevator: boolean = false;
  createRobot: boolean = false;
  deactivateRobot: boolean = false;
  createRobotType: boolean = false;
  createPassageway: boolean = false;
  patchMap: boolean = false;
  listPassagewaysBetween2Buildings: boolean = false;
  editPassageway: boolean = false;
  floorsWithPassageway: boolean = false;
  buildingsByFloorRange: boolean = false; 
  



  constructor(auth: AuthService) {
    this.isCampusManagerQuery().then((isCampusManager) => {
      this.isCampusManager = isCampusManager;
    });
    this.isAdministratorQuery().then((isAdministrator) => {
      this.isAdministrator = isAdministrator;
    });
    this.isFleetManagerQuery().then((isFleetManager) => {
      this.isFleetManager = isFleetManager;
    });
  }

  ngOnInit(): void {
  }

  public openSidebar(): void {
    const sidebar = document.querySelector('.sidebar') as HTMLElement;
    const content = document.getElementById('content') as HTMLElement;
    const toggleSidebarButton = document.getElementById('toggle-sidebar-button') as HTMLElement;

    if (!sidebar || !content || !toggleSidebarButton) {
      return;
    }

    if (!this.sidebarOpen) {
      sidebar.style.left = '0';
      content.style.marginLeft = '250px';
      toggleSidebarButton.style.left = '270px';
      toggleSidebarButton.style.display = 'block';
    } else {
      sidebar.style.left = '-250px';
      content.style.marginLeft = '0';
      toggleSidebarButton.style.display = 'none';
    }

    this.sidebarOpen = !this.sidebarOpen;
  }

  public closeSidebar(): void {
    const sidebar = document.querySelector('.sidebar') as HTMLElement;
    const content = document.getElementById('content') as HTMLElement;
    const toggleSidebarButton = document.getElementById('toggle-sidebar-button') as HTMLElement;

    if (!sidebar || !content || !toggleSidebarButton) {
      return;
    }

    sidebar.style.left = '-250px';
    content.style.marginLeft = '0';
    toggleSidebarButton.style.display = 'none';
    this.sidebarOpen = false;
  }

  public async logout(): Promise<void> {
    await this.auth.logout();
  }

  public async isCampusManagerQuery(): Promise<boolean> {
    const x = await this.auth.authenticatedUserRole();
    return x === Constants.default.CAMPUS_MANAGER_ROLE;
  }

  public async isAdministratorQuery(): Promise<boolean> {
    const x = await this.auth.authenticatedUserRole();
    return x === Constants.default.ADMIN_ROLE;
  }

  public async isFleetManagerQuery(): Promise<boolean> {
    const x = await this.auth.authenticatedUserRole();
    return x === Constants.default.FLEET_MANAGER_ROLE;
  }

  clear() {
    this.home_body = true;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.floorsByBuildingCode = false;
    this.deactivateRobot = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.createRobot = false;
    this.editPassageway = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  floorsByBuildingCodeList() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.floorsByBuildingCode = true;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.createElevator = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  buildingCreateForm() {
    this.home_body = false;
    this.listBuilding = false;
    this.editBuilding = false;
    this.createFloor = false;
    this.createBuilding = true;
    this.floorsByBuildingCode = false;
    this.deactivateRobot = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  floorCreateForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = true;
    this.editBuilding = false;
    this.listBuilding = false;
    this.floorsByBuildingCode = false;
    this.deactivateRobot = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  buildingEditForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = true;
    this.listBuilding = false;
    this.floorsByBuildingCode = false;
    this.deactivateRobot = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false; 
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  buildingList() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = true;
    this.floorsByBuildingCode = false;
    this.deactivateRobot = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  deactivateRobotForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = true;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  createRobotTypeForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = true;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  passagewayCreateForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = true;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  patchMapForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = true;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }
  

  editFloorForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = true;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  createElevatorForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.createElevator = true;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  listPassagewaysBetween2BuildingsForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = true;
    this.createElevator = false;
    this.editPassageway = false;
    this.createRobot = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  createRobotForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.createRobot = true;
    this.editPassageway = false;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;
  }

  editPassagewayForm() {
    this.home_body = false;
    this.createBuilding = false;
    this.createFloor = false;
    this.editBuilding = false;
    this.listBuilding = false;
    this.deactivateRobot = false;
    this.floorsByBuildingCode = false;
    this.createRobotType = false;
    this.createPassageway = false;
    this.patchMap = false;
    this.editFloor = false;
    this.listPassagewaysBetween2Buildings = false;
    this.createElevator = false;
    this.createRobot = false;
    this.editPassageway = true;
    this.floorsWithPassageway = false;
    this.buildingsByFloorRange = false;	
  }

  floorsWithPassagewayForm() {
      this.home_body = false;
      this.createBuilding = false;
      this.createFloor = false;
      this.editBuilding = false;
      this.listBuilding = false;
      this.deactivateRobot = false;
      this.floorsByBuildingCode = false;
      this.createRobotType = false;
      this.createPassageway = false;
      this.patchMap = false;
      this.editFloor = false;
      this.listPassagewaysBetween2Buildings = false;
      this.createElevator = false;
      this.createRobot = false;
      this.editPassageway = false;
      this.floorsWithPassageway = true;
      this.buildingsByFloorRange = false;
    }

    buildingsByFloorRangeForm() {
      this.home_body = false;
      this.createBuilding = false;
      this.createFloor = false;
      this.editBuilding = false;
      this.listBuilding = false;
      this.deactivateRobot = false;
      this.floorsByBuildingCode = false;
      this.createRobotType = false;
      this.createPassageway = false;
      this.patchMap = false;
      this.editFloor = false;
      this.listPassagewaysBetween2Buildings = false;
      this.createElevator = false;
      this.createRobot = false;
      this.editPassageway = false;
      this.floorsWithPassageway = false;
      this.buildingsByFloorRange = true;
    }
  
}
