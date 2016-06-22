package com.barcsys.http.registry

import com.barcsys.http.registry.Types.Service

/**
  * Store. persist types
  * Created by bigknife on 16/6/22.
  */
object Store {

  trait TypeStore {
    def saveService(service: Service): Either[Throwable, Service]
  }

}
