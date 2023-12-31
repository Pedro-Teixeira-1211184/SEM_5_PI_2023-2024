import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {PageNotFoundComponent} from "./page-not-found/page-not-found.component";
import {LoginComponent} from "./login/login.component";
import {AuthGuardService} from "./services/auth/auth-guard.service";
import {HomeComponent} from "./home/home.component";
import {RegisterComponent} from "./component/register/register.component";
import {DeleteComponent} from "./component/user/delete/delete.component";

const routes: Routes = [
    {path: '', redirectTo: '/login', pathMatch: 'full'},
    {path: 'login', component: LoginComponent},
    {path: 'register', component: RegisterComponent},
    {path: 'me', component: DeleteComponent, canActivate: [AuthGuardService]},
    {path: 'home', component: HomeComponent, canActivate: [AuthGuardService]},
    {path: '**', component: PageNotFoundComponent}
];

@NgModule({
    imports: [RouterModule.forRoot(routes)],
    declarations: [
    ],
    exports: [RouterModule]
})
export class AppRoutingModule {
}
