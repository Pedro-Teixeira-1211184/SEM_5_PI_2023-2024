import {Component, inject, OnInit} from '@angular/core';
import {RequestService} from "../../../services/user/request.service";
import {ISignUpRequestDTO} from "../../../dto/ISignUpRequestDTO";
import IRoleDTO from "../../../dto/IRoleDTO";
import {RoleService} from "../../../services/role/role.service";
import {AuthService} from "../../../services/auth.service";

@Component({
  selector: 'app-request',
  templateUrl: './request.component.html',
  styleUrls: ['./request.component.css']
})
export class RequestComponent implements OnInit {

  service: RequestService = inject(RequestService);
  r_service: RoleService = inject(RoleService);
  auth_service: AuthService = inject(AuthService);
  requests: (ISignUpRequestDTO & { role: string })[] = [];
  roles: IRoleDTO[] = [];

  constructor() {
  }

  ngOnInit(): void {
    this.getAllRequests();
    this.getAllRoles();
  }

  private async getAllRequests(): Promise<void> {
    this.requests = await this.service.getAllRequests();
  }

  private async getAllRoles(): Promise<void> {
    this.roles = await this.r_service.getAllRoles();
  }

  public async accept(request: ISignUpRequestDTO & { role: string }): Promise<void> {
    if (request.role != null || request.role != undefined) {
      await this.auth_service.signUp(request.firstName, request.lastName, request.nif , request.email, request.password, request.role);
    }
    //refresh content
    this.requests = [];
    await this.getAllRequests();
  }

  public async deny(request: ISignUpRequestDTO & { role: string }): Promise<void> {
    await this.auth_service.deleteRequest(request.email);
    //refresh content
    this.requests = [];
    await this.getAllRequests();
  }

}
